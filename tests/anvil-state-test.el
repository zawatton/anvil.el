;;; anvil-state-test.el --- Tests for anvil-state -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the Phase 1 anvil-state API: set / get / delete /
;; delete-ns / list-ns / count / vacuum, plus the TTL expiry and
;; non-readable-value guards described in
;; docs/design/08-state.org.
;;
;; Every test opens a fresh temp DB so there is no leakage between
;; runs.  A tiny fixture macro handles enable / disable / deletion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-state)

;;;; --- fixture ------------------------------------------------------------

(defmacro anvil-state-test--with-db (&rest body)
  "Run BODY against a fresh `anvil-state' DB.
Binds `anvil-state-db-path' to a unique temp file, closes any
pre-existing handle, enables the module for the duration of BODY,
and cleans up on exit."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-state-test-" nil ".db"))
         (anvil-state--db nil))
     (unwind-protect
         (progn
           (anvil-state-enable)
           ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

;;;; --- set / get round-trip -----------------------------------------------

(ert-deftest anvil-state-test-set-get-string ()
  "A string round-trips cleanly."
  (anvil-state-test--with-db
    (anvil-state-set "greeting" "hello")
    (should (equal "hello" (anvil-state-get "greeting")))))

(ert-deftest anvil-state-test-set-get-plist ()
  "A plist with mixed types round-trips."
  (anvil-state-test--with-db
    (let ((v (list :name "zaw" :count 3 :enabled t)))
      (anvil-state-set "cfg" v)
      (should (equal v (anvil-state-get "cfg"))))))

(ert-deftest anvil-state-test-set-get-hash-table ()
  "A hash-table value survives serialize / deserialize."
  (anvil-state-test--with-db
    (let ((h (make-hash-table :test 'equal)))
      (puthash "a" 1 h)
      (puthash "b" 2 h)
      (anvil-state-set "h" h)
      (let ((round (anvil-state-get "h")))
        (should (hash-table-p round))
        (should (= 1 (gethash "a" round)))
        (should (= 2 (gethash "b" round)))))))

(ert-deftest anvil-state-test-get-missing-returns-default ()
  "Absent keys return the `:default' argument."
  (anvil-state-test--with-db
    (should (null (anvil-state-get "nope")))
    (should (equal :placeholder
                   (anvil-state-get "nope" :default :placeholder)))))

;;;; --- namespaces ---------------------------------------------------------

(ert-deftest anvil-state-test-namespace-isolation ()
  "Same KEY in different NS do not collide."
  (anvil-state-test--with-db
    (anvil-state-set "k" "A" :ns "alpha")
    (anvil-state-set "k" "B" :ns "beta")
    (should (equal "A" (anvil-state-get "k" :ns "alpha")))
    (should (equal "B" (anvil-state-get "k" :ns "beta")))
    (should (equal (sort (copy-sequence '("alpha" "beta")) #'string<)
                   (anvil-state-list-ns)))))

(ert-deftest anvil-state-test-delete-ns-removes-all ()
  "`anvil-state-delete-ns' wipes an entire namespace."
  (anvil-state-test--with-db
    (anvil-state-set "a" 1 :ns "scratch")
    (anvil-state-set "b" 2 :ns "scratch")
    (anvil-state-set "c" 3 :ns "keep")
    (should (= 2 (anvil-state-delete-ns "scratch")))
    (should (null (anvil-state-get "a" :ns "scratch")))
    (should (= 3 (anvil-state-get "c" :ns "keep")))
    (should (equal '("keep") (anvil-state-list-ns)))))

;;;; --- delete / count -----------------------------------------------------

(ert-deftest anvil-state-test-delete-returns-t-then-nil ()
  "Delete reports t on first hit and nil on second."
  (anvil-state-test--with-db
    (anvil-state-set "x" 42)
    (should (eq t (anvil-state-delete "x")))
    (should (null (anvil-state-delete "x")))
    (should (null (anvil-state-get "x")))))

(ert-deftest anvil-state-test-count-total-and-per-ns ()
  "`anvil-state-count' works globally and per namespace."
  (anvil-state-test--with-db
    (anvil-state-set "a" 1 :ns "foo")
    (anvil-state-set "b" 2 :ns "foo")
    (anvil-state-set "c" 3 :ns "bar")
    (should (= 3 (anvil-state-count)))
    (should (= 2 (anvil-state-count "foo")))
    (should (= 1 (anvil-state-count "bar")))
    (should (= 0 (anvil-state-count "missing")))))

;;;; --- TTL ----------------------------------------------------------------

(ert-deftest anvil-state-test-ttl-expired-lazy-delete ()
  "Expired rows return default and are gone after the GET that notices."
  (anvil-state-test--with-db
    ;; Use the real TTL path so the 2 arguments go through `:ttl'.
    (anvil-state-set "k" "hot" :ttl 1)
    ;; Force the row into the past by rewriting expires_at directly —
    ;; avoids sleeping in the test.
    (sqlite-execute
     (anvil-state--db)
     "UPDATE kv SET expires_at = 1 WHERE ns = ?1 AND k = ?2"
     (list anvil-state-default-namespace "k"))
    (should (null (anvil-state-get "k")))
    (should (= 0 (anvil-state-count)))))

(ert-deftest anvil-state-test-ttl-invalid-ttl-errors ()
  "Non-positive TTL is rejected before touching the DB."
  (anvil-state-test--with-db
    (should-error (anvil-state-set "k" "v" :ttl 0) :type 'user-error)
    (should-error (anvil-state-set "k" "v" :ttl -5) :type 'user-error)))

;;;; --- serialization guards -----------------------------------------------

(ert-deftest anvil-state-test-non-readable-rejected ()
  "Buffer objects fail at SET with a user-error, before writing."
  (anvil-state-test--with-db
    (let ((buf (generate-new-buffer " *anvil-state-test*")))
      (unwind-protect
          (should-error (anvil-state-set "buf" buf) :type 'user-error)
        (kill-buffer buf)))
    (should (= 0 (anvil-state-count)))))

(ert-deftest anvil-state-test-empty-key-rejected ()
  "Empty-string KEY is rejected."
  (anvil-state-test--with-db
    (should-error (anvil-state-set "" "v") :type 'user-error)
    (should-error (anvil-state-get "") :type 'user-error)))

;;;; --- vacuum -------------------------------------------------------------

(ert-deftest anvil-state-test-vacuum-removes-expired ()
  "`anvil-state-vacuum' deletes expired rows and reports the count."
  (anvil-state-test--with-db
    (anvil-state-set "live" 1 :ttl 3600)
    (anvil-state-set "dead" 2 :ttl 1)
    (sqlite-execute (anvil-state--db)
                    "UPDATE kv SET expires_at = 1 WHERE k = ?1"
                    (list "dead"))
    (let ((res (anvil-state-vacuum)))
      (should (= 1 (plist-get res :expired)))
      (should (numberp (plist-get res :size-before)))
      (should (numberp (plist-get res :size-after))))
    (should (= 1 (anvil-state-count)))
    (should (= 1 (anvil-state-get "live")))))

;;;; --- persistence across reopen -----------------------------------------

(ert-deftest anvil-state-test-persistence-across-reopen ()
  "Values survive `disable' + `enable' (backed by disk, not memory)."
  (anvil-state-test--with-db
    (anvil-state-set "persist" "yes" :ns "soak")
    ;; Close and reopen the DB; the fixture keeps the path stable.
    (anvil-state-disable)
    (anvil-state-enable)
    (should (equal "yes" (anvil-state-get "persist" :ns "soak")))))

(provide 'anvil-state-test)
;;; anvil-state-test.el ends here
