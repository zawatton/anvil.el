;;; anvil-http-test.el --- Tests for anvil-http -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `anvil-http' that stub `anvil-http--request' so no
;; real network traffic happens.  One live smoke test at the bottom
;; fetches https://example.com when ANVIL_ALLOW_LIVE=1 is set.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-http)
(require 'anvil-state)

;;;; --- fixtures -----------------------------------------------------------

(defvar anvil-http-test--calls nil
  "List of `(:method :url :headers :timeout)' recorded by the stub,
newest first.")

(defvar anvil-http-test--responses nil
  "Queue of fake response plists to hand back to successive calls.")

(defun anvil-http-test--pop-response ()
  (or (pop anvil-http-test--responses)
      (error "anvil-http-test: response queue exhausted")))

(defmacro anvil-http-test--with-stub (responses &rest body)
  "Run BODY with `anvil-http--request' returning successive RESPONSES.
Provides a fresh `anvil-state' DB and zeroed metrics."
  (declare (indent 1))
  `(let ((anvil-http-test--calls nil)
         (anvil-http-test--responses (copy-sequence ,responses))
         (anvil-state-db-path (make-temp-file "anvil-http-st-" nil ".db"))
         (anvil-state--db nil)
         (anvil-http--metrics
          (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                :network-200 0 :errors 0 :log nil)))
     (unwind-protect
         (progn
           (anvil-state-enable)
           (cl-letf (((symbol-function 'anvil-http--request)
                      (lambda (method url headers timeout)
                        (push (list :method method :url url
                                    :headers headers :timeout timeout)
                              anvil-http-test--calls)
                        (anvil-http-test--pop-response)))
                     ((symbol-function 'sleep-for)
                      (lambda (&rest _) nil)))
             ,@body))
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(defun anvil-http-test--response (status &optional headers body final-url)
  (list :status status
        :headers headers
        :body (or body "")
        :final-url (or final-url "https://example.com/")))

;;;; --- URL validation -----------------------------------------------------

(ert-deftest anvil-http-test-check-url-rejects-empty ()
  "Empty or non-string URL → user-error."
  (should-error (anvil-http-get "") :type 'user-error)
  (should-error (anvil-http-get nil) :type 'user-error))

(ert-deftest anvil-http-test-check-url-rejects-bad-scheme ()
  "file:// / javascript: scheme are refused."
  (should-error (anvil-http-get "file:///etc/passwd") :type 'user-error)
  (should-error (anvil-http-get "javascript:alert(1)") :type 'user-error))

(ert-deftest anvil-http-test-normalize-url-lowercases-and-strips-fragment ()
  (should (equal "https://example.com/path?q=1"
                 (anvil-http--normalize-url
                  "HTTPS://Example.COM/path?q=1#section"))))

;;;; --- happy path GET -----------------------------------------------------

(ert-deftest anvil-http-test-get-returns-body-and-status ()
  "200 response surfaces status/headers/body and records a cache entry."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :etag "\"abc\"" :content-type "text/html")
             "<h1>hi</h1>"
             "https://example.com/"))
    (let ((resp (anvil-http-get "https://example.com/" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (string-match-p "<h1>hi</h1>" (plist-get resp :body)))
      (should-not (plist-get resp :from-cache))
      (let ((entry (anvil-http--cache-get
                    (anvil-http--normalize-url "https://example.com/"))))
        (should entry)
        (should (equal "\"abc\"" (plist-get entry :etag)))))))

;;;; --- cache fresh-serve --------------------------------------------------

(ert-deftest anvil-http-test-ttl-fresh-hit-avoids-network ()
  "Second GET within TTL serves from cache with zero requests."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :etag "\"x\"") "body-v1"
             "https://example.com/"))
    (anvil-http-get "https://example.com/" :cache-ttl-sec 600)
    (let ((before (length anvil-http-test--calls))
          (resp (anvil-http-get "https://example.com/" :cache-ttl-sec 600)))
      (should (= before (length anvil-http-test--calls)))
      (should (plist-get resp :from-cache))
      (should (string= "body-v1" (plist-get resp :body)))
      (should (= 1 (plist-get anvil-http--metrics :cache-fresh))))))

;;;; --- conditional revalidation / 304 -------------------------------------

(ert-deftest anvil-http-test-304-serves-cached-body ()
  "Expired TTL triggers conditional GET; 304 serves cached body."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :etag "\"v1\"" :last-modified "Wed, 01 Jan 2025 00:00:00 GMT")
             "first-body" "https://example.com/")
            (anvil-http-test--response 304 nil "" "https://example.com/"))
    ;; Seed the cache with TTL=0 so next request revalidates.
    (anvil-http-get "https://example.com/" :cache-ttl-sec 0)
    (let ((resp (anvil-http-get "https://example.com/" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (plist-get resp :from-cache))
      (should (string= "first-body" (plist-get resp :body))))
    (let* ((revalidate-call (car anvil-http-test--calls))
           (headers (plist-get revalidate-call :headers)))
      (should (equal "\"v1\"" (cdr (assoc "If-None-Match" headers))))
      (should (assoc "If-Modified-Since" headers)))
    (should (= 1 (plist-get anvil-http--metrics :cache-revalidated)))))

;;;; --- if-newer-than forwarded as If-Modified-Since ------------------------

(ert-deftest anvil-http-test-if-newer-than-sends-ims ()
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "body"))
    (anvil-http-get "https://example.com/" :if-newer-than 1700000000)
    (let ((headers (plist-get (car anvil-http-test--calls) :headers)))
      (should (assoc "If-Modified-Since" headers)))))

;;;; --- retry on 5xx --------------------------------------------------------

(ert-deftest anvil-http-test-retry-on-500-then-success ()
  "500 is retried; second attempt's 200 is returned."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 500 nil "")
            (anvil-http-test--response 200 nil "ok"))
    (let ((resp (anvil-http-get "https://example.com/" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (= 2 (length anvil-http-test--calls))))))

(ert-deftest anvil-http-test-no-retry-on-404 ()
  "4xx (non-408/429) is NOT retried and surfaces as tool-error."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 404 nil ""))
    (should-error (anvil-http-get "https://example.com/404")
                  :type 'anvil-server-tool-error)
    (should (= 1 (length anvil-http-test--calls)))
    (should (= 1 (plist-get anvil-http--metrics :errors)))))

(ert-deftest anvil-http-test-429-retry-after-numeric ()
  "429 with numeric Retry-After is honoured before retry."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 429 (list :retry-after "2") "")
            (anvil-http-test--response 200 nil "ok"))
    (let ((resp (anvil-http-get "https://example.com/rate" :cache-ttl-sec 0)))
      (should (= 200 (plist-get resp :status)))
      (should (= 2 (length anvil-http-test--calls))))))

;;;; --- user-agent / Accept-Encoding injection -----------------------------

(ert-deftest anvil-http-test-user-agent-and-accept-encoding ()
  "Default UA + Accept-Encoding gzip are injected when absent.
The real `anvil-http--request' inserts these — verified by letting it run
in a fake url-retrieve."
  (cl-letf* ((captured nil)
             ((symbol-function 'url-retrieve-synchronously)
              (lambda (&rest _)
                (setq captured url-request-extra-headers)
                ;; Return a buffer shaped like a minimal url-http response.
                (with-current-buffer (generate-new-buffer " *anvil-http-fake*")
                  (insert "HTTP/1.1 200 OK\r\n"
                          "Content-Type: text/plain\r\n"
                          "\r\n"
                          "ok")
                  (goto-char (point-min))
                  (re-search-forward "\r\n\r\n")
                  (setq-local url-http-end-of-headers
                              (copy-marker (match-end 0)))
                  (setq-local url-http-target-url "https://example.com/")
                  (current-buffer)))))
    (let ((anvil-state-db-path (make-temp-file "anvil-http-ua-" nil ".db"))
          (anvil-state--db nil)
          (anvil-http--metrics
           (list :requests 0 :cache-fresh 0 :cache-revalidated 0
                 :network-200 0 :errors 0 :log nil)))
      (unwind-protect
          (progn
            (anvil-state-enable)
            (anvil-http-get "https://example.com/" :cache-ttl-sec 0)
            (should (assoc "User-Agent" captured))
            (should (string-match-p "^anvil\\.el/"
                                    (cdr (assoc "User-Agent" captured))))
            (should (equal "gzip" (cdr (assoc "Accept-Encoding" captured)))))
        (anvil-state-disable)
        (ignore-errors (delete-file anvil-state-db-path))))))

;;;; --- HEAD ---------------------------------------------------------------

(ert-deftest anvil-http-test-head-does-not-cache ()
  "HEAD bypasses the cache on both read and write."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200 (list :content-length "42") ""))
    (let ((resp (anvil-http-head "https://example.com/")))
      (should (= 200 (plist-get resp :status)))
      (should (equal "42" (plist-get (plist-get resp :headers) :content-length)))
      (should-not (anvil-http--cache-get
                   (anvil-http--normalize-url "https://example.com/"))))))

;;;; --- cache clear --------------------------------------------------------

(ert-deftest anvil-http-test-cache-clear-by-url ()
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "body"))
    (anvil-http-get "https://example.com/page" :cache-ttl-sec 600)
    (should (anvil-http--cache-get
             (anvil-http--normalize-url "https://example.com/page")))
    (should (= 1 (anvil-http-cache-clear "https://example.com/page")))
    (should-not (anvil-http--cache-get
                 (anvil-http--normalize-url "https://example.com/page")))))

(ert-deftest anvil-http-test-cache-clear-all ()
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "a")
            (anvil-http-test--response 200 nil "b"))
    (anvil-http-get "https://example.com/a" :cache-ttl-sec 600)
    (anvil-http-get "https://example.com/b" :cache-ttl-sec 600)
    (should (>= (anvil-http-cache-clear) 2))
    (should-not (anvil-http--cache-get
                 (anvil-http--normalize-url "https://example.com/a")))))

;;;; --- MCP tool argument coercion -----------------------------------------

(ert-deftest anvil-http-test-tool-fetch-coerces-string-timeout ()
  "MCP params arrive as strings; the tool coerces digit strings."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response 200 nil "ok"))
    (anvil-http--tool-fetch "https://example.com/" nil nil "42" nil)
    (should (= 42 (plist-get (car anvil-http-test--calls) :timeout)))))

;;;; --- Phase 1b: selector / json-path extract -----------------------------

(defvar anvil-http-test--html-fixture
  "<html><body>
  <h1>Hello</h1>
  <div class=\"greet\">Welcome</div>
  <div id=\"main\">Main content</div>
  <p class=\"note\">alpha</p>
  <p class=\"note\">beta</p>
</body></html>"
  "Minimal HTML payload exercised by the selector tests.")

(defun anvil-http-test--make-resp (ct body)
  "Return a minimal response plist with content-type CT and body BODY."
  (list :status 200
        :headers (list :content-type ct)
        :body body
        :from-cache nil
        :cached-at nil
        :final-url "https://example.com/"
        :elapsed-ms 1))

(ert-deftest anvil-http-test-phase1b-select-html-libxml-tag ()
  "libxml path: `h1' selector extracts the heading text only."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((resp (anvil-http-test--make-resp
                "text/html; charset=utf-8" anvil-http-test--html-fixture))
         (out (anvil-http--apply-extract resp "h1" nil)))
    (should (eq 'selector (plist-get out :extract-mode)))
    (should (eq 'libxml (plist-get out :extract-engine)))
    (should (string-match-p "\\`Hello" (plist-get out :body)))
    (should-not (plist-get out :extract-miss))))

(ert-deftest anvil-http-test-phase1b-select-html-libxml-tag-class ()
  "libxml path: `p.note' picks every paragraph with class note."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let* ((resp (anvil-http-test--make-resp
                "text/html" anvil-http-test--html-fixture))
         (out (anvil-http--apply-extract resp "p.note" nil))
         (body (plist-get out :body)))
    (should (string-match-p "alpha" body))
    (should (string-match-p "beta" body))
    (should-not (string-match-p "Welcome" body))
    (should (eq 'libxml (plist-get out :extract-engine)))))

(ert-deftest anvil-http-test-phase1b-select-html-fallback ()
  "Regex fallback runs when libxml-p reports nil and still matches
the shared subset (`#main')."
  (cl-letf (((symbol-function 'anvil-http--libxml-p)
             (lambda () nil)))
    (let* ((resp (anvil-http-test--make-resp
                  "text/html" anvil-http-test--html-fixture))
           (out (anvil-http--apply-extract resp "#main" nil)))
      (should (eq 'regex-subset (plist-get out :extract-engine)))
      (should (equal "Main content" (plist-get out :body)))
      (should-not (plist-get out :extract-miss)))))

(ert-deftest anvil-http-test-phase1b-select-html-miss ()
  "Selector that matches nothing → `:extract-miss' t, full body preserved."
  (let* ((resp (anvil-http-test--make-resp
                "text/html" anvil-http-test--html-fixture))
         (out (anvil-http--apply-extract resp "nosuch" nil)))
    (should (eq t (plist-get out :extract-miss)))
    (should (eq 'selector (plist-get out :extract-mode)))
    (should (string-match-p "Welcome" (plist-get out :body)))))

(ert-deftest anvil-http-test-phase1b-select-json-nested ()
  "json-path walks nested keys; the extracted node is re-serialized."
  (let* ((body "{\"data\":{\"count\":42,\"label\":\"ok\"}}")
         (resp (anvil-http-test--make-resp "application/json" body))
         (out (anvil-http--apply-extract resp nil "data.count")))
    (should (eq 'json-path (plist-get out :extract-mode)))
    (should (equal "42" (plist-get out :body)))
    (should-not (plist-get out :extract-miss))))

(ert-deftest anvil-http-test-phase1b-select-json-wildcard ()
  "`items[*].id' flattens an array; result round-trips as a JSON array."
  (let* ((body "{\"items\":[{\"id\":\"a\"},{\"id\":\"b\"},{\"id\":\"c\"}]}")
         (resp (anvil-http-test--make-resp "application/json" body))
         (out (anvil-http--apply-extract resp nil "items[*].id"))
         (decoded (json-parse-string (plist-get out :body)
                                     :array-type 'array)))
    (should (equal 3 (length decoded)))
    (should (equal "a" (aref decoded 0)))
    (should (equal "c" (aref decoded 2)))))

(ert-deftest anvil-http-test-phase1b-extract-content-type-mismatch ()
  "Selector supplied but body is JSON → mismatch engine + extract-miss."
  (let* ((resp (anvil-http-test--make-resp
                "application/json" "{\"a\":1}"))
         (out (anvil-http--apply-extract resp "h1" nil)))
    (should (eq t (plist-get out :extract-miss)))
    (should (eq 'selector (plist-get out :extract-mode)))
    (should (eq 'content-type-mismatch
                (plist-get out :extract-engine)))))

;;;; --- Phase 1c: overflow + header-filter ---------------------------------

(defmacro anvil-http-test--with-overflow-dir (dir-var &rest body)
  "Bind DIR-VAR to a fresh temp dir and delete it after BODY."
  (declare (indent 1))
  `(let ((,dir-var (make-temp-file "anvil-http-overflow-" t)))
     (unwind-protect
         (let ((anvil-http-overflow-dir ,dir-var))
           ,@body)
       (ignore-errors (delete-directory ,dir-var t)))))

(ert-deftest anvil-http-test-phase1c-overflow-spill ()
  "Body over `anvil-http-max-inline-body-bytes' spills to a hashed
tempfile; response carries head slice + overflow metadata."
  (anvil-http-test--with-overflow-dir dir
    (anvil-http-test--with-stub
        (list (anvil-http-test--response
               200
               (list :content-type "text/plain")
               (make-string 250000 ?a)))
      (let* ((anvil-http-max-inline-body-bytes 200000)
             (anvil-http-overflow-head-bytes 64)
             (r (anvil-http-get "https://example.com/big")))
        (should (eq t (plist-get r :body-truncated)))
        (should (= 64 (length (plist-get r :body))))
        (should (= 250000 (plist-get r :total-bytes)))
        (should (stringp (plist-get r :body-overflow-path)))
        (should (file-exists-p (plist-get r :body-overflow-path)))
        (should (= 64 (length (make-string 64 ?a))))
        (should (stringp (plist-get r :body-sha256)))
        (should (eq 'overflow (plist-get r :body-mode)))))))

(ert-deftest anvil-http-test-phase1c-body-mode-full ()
  "`body-mode full' inlines the body even when over threshold."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "text/plain")
             (make-string 250000 ?a)))
    (let* ((anvil-http-max-inline-body-bytes 1000)
           (r (anvil-http-get "https://example.com/big" :body-mode 'full)))
      (should-not (plist-get r :body-truncated))
      (should-not (plist-get r :body-overflow-path))
      (should (= 250000 (length (plist-get r :body)))))))

(ert-deftest anvil-http-test-phase1c-body-mode-meta-only ()
  "`body-mode meta-only' drops :body entirely; metadata survives."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "text/plain" :etag "\"w1\"")
             "hello"))
    (let ((r (anvil-http-get "https://example.com/m" :body-mode 'meta-only)))
      (should (null (plist-get r :body)))
      (should (eq 'meta-only (plist-get r :body-mode)))
      (should (= 200 (plist-get r :status)))
      ;; ETag must survive so the caller can pass it back next time.
      (should (equal "\"w1\"" (plist-get (plist-get r :headers) :etag))))))

(ert-deftest anvil-http-test-phase1c-header-filter-minimal ()
  "Default header-filter keeps only `anvil-http-minimal-header-keys'."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "application/json"
                   :etag "\"abc\""
                   :server "nginx/1.19"
                   :x-cache "HIT"
                   :date "Thu, 01 Jan 1970 00:00:00 GMT")
             "{\"x\":1}"))
    (let* ((anvil-http-header-filter-default 'minimal)
           (r (anvil-http-get "https://example.com/j"))
           (h (plist-get r :headers)))
      (should (equal "application/json" (plist-get h :content-type)))
      (should (equal "\"abc\"" (plist-get h :etag)))
      (should (null (plist-get h :server)))
      (should (null (plist-get h :x-cache)))
      (should (null (plist-get h :date))))))

(ert-deftest anvil-http-test-phase1c-header-filter-all ()
  "`header-filter all' opt-out returns every header the server sent."
  (anvil-http-test--with-stub
      (list (anvil-http-test--response
             200
             (list :content-type "application/json"
                   :server "nginx/1.19"
                   :x-ratelimit-limit "60")
             "{}"))
    (let* ((anvil-http-header-filter-default 'minimal)
           (r (anvil-http-get "https://example.com/j"
                              :header-filter 'all))
           (h (plist-get r :headers)))
      (should (equal "nginx/1.19" (plist-get h :server)))
      (should (equal "60" (plist-get h :x-ratelimit-limit))))))

;;;; --- live smoke test ----------------------------------------------------

(ert-deftest anvil-http-test-live-example-com ()
  "Hit the real server when ANVIL_ALLOW_LIVE=1."
  (skip-unless (and (not (getenv "ANVIL_SKIP_LIVE"))
                    (getenv "ANVIL_ALLOW_LIVE")))
  (let ((anvil-state-db-path (make-temp-file "anvil-http-live-" nil ".db"))
        (anvil-state--db nil)
        (anvil-http--metrics
         (list :requests 0 :cache-fresh 0 :cache-revalidated 0
               :network-200 0 :errors 0 :log nil)))
    (unwind-protect
        (progn
          (anvil-state-enable)
          (let ((resp (anvil-http-get "https://example.com/"
                                      :cache-ttl-sec 0)))
            (should (= 200 (plist-get resp :status)))
            (should (string-match-p "Example Domain"
                                    (plist-get resp :body)))))
      (anvil-state-disable)
      (ignore-errors (delete-file anvil-state-db-path)))))

(provide 'anvil-http-test)
;;; anvil-http-test.el ends here
