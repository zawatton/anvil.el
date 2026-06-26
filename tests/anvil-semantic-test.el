;;; anvil-semantic-test.el --- Tests for anvil-semantic -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for term extraction and the org / plain-text chunkers, plus
;; FTS5 search smoke tests (skipped gracefully when built-in SQLite is
;; unavailable).  Japanese strings are built from Unicode code points so
;; the suite is independent of how the host loads this source file —
;; important on Windows where `emacs --batch -Q' may not decode source as
;; UTF-8.  The 2-char-compound case is the regression guard for the
;; trigram-MATCH + LIKE-fallback retrieval design.

;;; Code:

(require 'ert)
(require 'json)
(require 'anvil-semantic)

;;;; Japanese helpers (code points → string, ASCII-only source)

(defun anvil-semantic-test--ja (&rest cps)
  "Return the string of Unicode code points CPS."
  (apply #'string cps))

(defconst anvil-semantic-test--seitei            ; 整定
  (anvil-semantic-test--ja #x6574 #x5b9a))
(defconst anvil-semantic-test--rouden            ; 漏電
  (anvil-semantic-test--ja #x6f0f #x96fb))
(defconst anvil-semantic-test--rouei-denryu      ; 漏洩電流
  (anvil-semantic-test--ja #x6f0f #x6d29 #x96fb #x6d41))
(defconst anvil-semantic-test--rouden-kenshutsu  ; 漏電検出
  (anvil-semantic-test--ja #x6f0f #x96fb #x691c #x51fa))
(defconst anvil-semantic-test--setsuchi          ; 接地
  (anvil-semantic-test--ja #x63a5 #x5730))
(defconst anvil-semantic-test--konkyo            ; 根拠
  (anvil-semantic-test--ja #x6839 #x62e0))
(defconst anvil-semantic-test--no                ; の
  (anvil-semantic-test--ja #x306e))
(defconst anvil-semantic-test--maru              ; 。
  (anvil-semantic-test--ja #x3002))

;;;; Term extraction

(ert-deftest anvil-semantic-test-terms-ascii ()
  "Alnum identifiers are lowercased; short noise is dropped."
  (should (equal '("leakage" "detection")
                 (anvil-semantic--terms "Leakage DETECTION")))
  ;; single chars below the length-2 floor are ignored
  (should (null (anvil-semantic--terms "a b c"))))

(ert-deftest anvil-semantic-test-terms-cjk-runs ()
  "Kanji runs of >=2 chars become terms; mixed ascii+kanji split."
  (should (equal (list anvil-semantic-test--seitei)
                 (anvil-semantic--terms anvil-semantic-test--seitei)))
  (should (equal (list "ovgr" anvil-semantic-test--seitei)
                 (anvil-semantic--terms
                  (concat "OVGR" anvil-semantic-test--seitei)))))

(ert-deftest anvil-semantic-test-merge-terms-synonyms ()
  "Comma-separated synonyms are merged in, de-duplicated."
  (let ((m (anvil-semantic--merge-terms
            anvil-semantic-test--rouden
            (concat anvil-semantic-test--rouei-denryu ","
                    anvil-semantic-test--rouden))))   ; dup of query term
    (should (member anvil-semantic-test--rouden m))
    (should (member anvil-semantic-test--rouei-denryu m))
    (should (= (length m) 2))))

;;;; Chunkers

(defun anvil-semantic-test--write (path content)
  "Write CONTENT to PATH as UTF-8 and return PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path (insert content)))
  path)

(ert-deftest anvil-semantic-test-chunk-org-heading-context ()
  "Org chunks carry a `title :: heading' context prefix and start line."
  (let* ((dir (make-temp-file "anvil-sem-org" t))
         (file (expand-file-name "f.org" dir))
         (body (concat anvil-semantic-test--seitei anvil-semantic-test--no
                       anvil-semantic-test--konkyo anvil-semantic-test--maru
                       anvil-semantic-test--rouden-kenshutsu))
         (content (concat "#+TITLE: Denki\n\n* OVGR\n" body "\n"
                          "* " anvil-semantic-test--setsuchi "\n"
                          anvil-semantic-test--rouei-denryu "\n")))
    (unwind-protect
        (let* ((chunks (anvil-semantic--chunk-org
                        (anvil-semantic-test--write file content)))
               (ovgr (cl-find "OVGR" chunks :key #'car :test #'equal)))
          (should ovgr)
          ;; start line of the OVGR heading is line 3
          (should (= 3 (nth 1 ovgr)))
          ;; context prefix = "Denki :: OVGR", then body containing 整定
          (should (string-prefix-p "Denki :: OVGR" (nth 2 ovgr)))
          (should (string-search anvil-semantic-test--seitei (nth 2 ovgr))))
      (delete-directory dir t))))

(ert-deftest anvil-semantic-test-chunk-text-line-windows ()
  "Plain-text chunking splits into line windows with nil heading."
  (let* ((dir (make-temp-file "anvil-sem-txt" t))
         (file (expand-file-name "f.txt" dir))
         (anvil-semantic-chunk-lines 2)
         (anvil-semantic-min-chars 1)
         (content "alpha line one\nbeta line two\ngamma line three\n"))
    (unwind-protect
        (let ((chunks (anvil-semantic--chunk-text
                       (anvil-semantic-test--write file content))))
          (should (= 2 (length chunks)))          ; 3 lines / window 2
          (should (null (car (nth 0 chunks))))    ; heading nil
          (should (= 1 (nth 1 (nth 0 chunks))))   ; first window at line 1
          (should (= 3 (nth 1 (nth 1 chunks)))))  ; second window at line 3
      (delete-directory dir t))))

;;;; Search smoke tests (require built-in SQLite)

(defmacro anvil-semantic-test--with-corpus (&rest body)
  "Build a temp corpus + fresh DB, bind anvil-semantic state, run BODY."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (anvil-semantic--available-p))
     (let* ((dir (make-temp-file "anvil-sem-corpus" t))
            (anvil-semantic-roots (list dir))
            (anvil-semantic-db-path (expand-file-name "idx.db" dir))
            (anvil-semantic--db nil))
       (unwind-protect
           (let ((org (expand-file-name "denki.org" dir))
                 (txt (expand-file-name "notes.txt" dir)))
             (anvil-semantic-test--write
              org
              (concat "#+TITLE: Denki\n\n* OVGR\n"
                      anvil-semantic-test--seitei anvil-semantic-test--no
                      anvil-semantic-test--konkyo anvil-semantic-test--maru
                      anvil-semantic-test--rouden-kenshutsu "\n\n* "
                      anvil-semantic-test--setsuchi "\n"
                      ;; body must clear `anvil-semantic-min-chars' (12) to be
                      ;; indexed; include 漏洩電流 for the synonym-recall test.
                      anvil-semantic-test--setsuchi anvil-semantic-test--konkyo
                      anvil-semantic-test--no anvil-semantic-test--rouei-denryu
                      anvil-semantic-test--no anvil-semantic-test--rouden-kenshutsu
                      "\n"))
             (anvil-semantic-test--write
              txt "earth leakage detection threshold tuning here\n")
             (anvil-semantic-rebuild)
             ,@body)
         (when (and anvil-semantic--db (sqlitep anvil-semantic--db))
           (ignore-errors (sqlite-close anvil-semantic--db)))
         (delete-directory dir t)))))

(ert-deftest anvil-semantic-test-rebuild-counts ()
  "Rebuild indexes both files into several chunks."
  (anvil-semantic-test--with-corpus
    (let ((db (anvil-semantic--ensure-db)))
      (should (= 2 (caar (sqlite-select
                          db "SELECT COUNT(DISTINCT file) FROM chunk_fts"))))
      (should (>= (caar (sqlite-select db "SELECT COUNT(*) FROM chunk_fts")) 3)))))

(ert-deftest anvil-semantic-test-search-ascii-bm25 ()
  "An ASCII multi-word query matches via FTS5 and carries a BM25 score."
  (anvil-semantic-test--with-corpus
    (let* ((hits (anvil-semantic--search "leakage detection" 5 nil "hybrid" nil)))
      (should hits)
      (should (cl-find-if (lambda (h) (plist-get h :bm25)) hits))
      (should (= 2 (plist-get (car hits) :lex))))))

(ert-deftest anvil-semantic-test-search-2char-cjk-like-fallback ()
  "A 2-char kanji query (trigram MATCH = 0) still hits via LIKE fallback.
This is the core regression guard for the retrieval design."
  (anvil-semantic-test--with-corpus
    (let ((db (anvil-semantic--ensure-db))
          (terms (list anvil-semantic-test--seitei)))
      ;; trigram MATCH alone finds nothing for a 2-char term ...
      (should (null (anvil-semantic--match-candidates db terms nil 50)))
      ;; ... but the LIKE pass does ...
      (should (anvil-semantic--like-candidates db terms nil 50))
      ;; ... so the end-to-end search returns a hit.
      (should (anvil-semantic--search
               anvil-semantic-test--seitei 5 nil "hybrid" nil)))))

(ert-deftest anvil-semantic-test-search-synonym-expansion ()
  "notes-lexical-search recall widens via comma synonyms."
  (anvil-semantic-test--with-corpus
    ;; 漏洩電流 appears only in the 接地 section; reach it via `terms'.
    (let ((hits (anvil-semantic--search
                 anvil-semantic-test--rouden       ; query: 漏電
                 5 nil "lexical"
                 (anvil-semantic--merge-terms
                  anvil-semantic-test--rouden
                  anvil-semantic-test--rouei-denryu))))
      (should hits)
      (should (cl-find-if
               (lambda (h) (string-search anvil-semantic-test--rouei-denryu
                                          (plist-get h :text)))
               hits)))))

(ert-deftest anvil-semantic-test-root-filter ()
  "A root prefix restricts results to files under it."
  (anvil-semantic-test--with-corpus
    (let* ((sub (file-name-directory (expand-file-name "denki.org" dir)))
           (hits (anvil-semantic--search "leakage detection" 5 sub "hybrid" nil)))
      ;; notes.txt is under the same dir, so it still matches; a bogus root
      ;; yields nothing.
      (should hits)
      (should (null (anvil-semantic--search
                     "leakage detection" 5
                     (expand-file-name "no-such-dir" dir) "hybrid" nil))))))

(ert-deftest anvil-semantic-test-refresh-incremental ()
  "Adding a file and refreshing indexes it without a full rebuild."
  (anvil-semantic-test--with-corpus
    (let ((db (anvil-semantic--ensure-db))
          (before (caar (sqlite-select
                         (anvil-semantic--ensure-db)
                         "SELECT COUNT(DISTINCT file) FROM chunk_fts"))))
      (anvil-semantic-test--write
       (expand-file-name "extra.txt" dir)
       "an additional document about grounding resistance\n")
      (anvil-semantic-refresh-if-stale)
      (should (= (1+ before)
                 (caar (sqlite-select
                        db "SELECT COUNT(DISTINCT file) FROM chunk_fts")))))))

(ert-deftest anvil-semantic-test-tool-empty-query ()
  "The search tool returns an empty JSON array for an empty query."
  (anvil-semantic-test--with-corpus
    (should (equal "[]" (anvil-semantic--tool-search "" "5")))
    ;; A real query returns parseable JSON with the expected keys.
    (let* ((out (anvil-semantic--tool-search "leakage detection" "5"))
           (arr (json-parse-string out :object-type 'alist)))
      (should (> (length arr) 0))
      (should (assq 'file (aref arr 0)))
      (should (assq 'snippet (aref arr 0))))))

;;;; Embedding (vector) layer — network-free tests

(ert-deftest anvil-semantic-test-embed-model-defaults ()
  "The active embed model falls back to a per-provider default."
  (let ((anvil-semantic-embed-model nil))
    (let ((anvil-semantic-embed-provider 'ollama))
      (should (equal "nomic-embed-text" (anvil-semantic--embed-model))))
    (let ((anvil-semantic-embed-provider 'openai))
      (should (equal "text-embedding-3-small" (anvil-semantic--embed-model)))))
  (let ((anvil-semantic-embed-model "custom")
        (anvil-semantic-embed-provider 'ollama))
    (should (equal "custom" (anvil-semantic--embed-model)))))

(ert-deftest anvil-semantic-test-vec-roundtrip-and-cosine ()
  "Vectors round-trip through TEXT and normalize/dot give cosine."
  (let* ((v (vector 3.0 4.0))
         (back (anvil-semantic--text->vec (anvil-semantic--vec->text v))))
    (should (equal back v))
    (let ((u (anvil-semantic--normalize v)))
      ;; unit norm: dot(u,u) = 1
      (should (< (abs (- 1.0 (anvil-semantic--dot u u))) 1e-6))
      (should (< (abs (- 0.6 (aref u 0))) 1e-6))
      (should (< (abs (- 0.8 (aref u 1))) 1e-6))
      ;; orthogonal vectors have cosine 0
      (should (< (abs (anvil-semantic--dot u (anvil-semantic--normalize
                                              (vector -4.0 3.0))))
                 1e-6)))))

(defun anvil-semantic-test--put-vec (db digest file model v)
  "Store normalized vector V for DIGEST/FILE under MODEL in chunk_vec."
  (anvil-semantic--ensure-vec-schema db)
  (sqlite-execute
   db "INSERT OR REPLACE INTO chunk_vec(digest,model,file,dim,vec)
        VALUES (?,?,?,?,?)"
   (list digest model file (length v) (anvil-semantic--vec->text v))))

(ert-deftest anvil-semantic-test-embed-available-gate ()
  "An empty chunk_vec keeps the engine off the vector path; status shows it."
  (anvil-semantic-test--with-corpus
    (let ((db (anvil-semantic--ensure-db))
          (model (anvil-semantic--embed-model)))
      (should-not (anvil-semantic--embed-available-p db model))
      (anvil-semantic-test--put-vec
       db "deadbeef" "/x" model (anvil-semantic--normalize (vector 1.0 0.0)))
      (should (anvil-semantic--embed-available-p db model))
      (let ((s (anvil-semantic--tool-status)))
        (should (string-search ":vectors" s))
        (should (string-search ":embed-provider" s))))))

(ert-deftest anvil-semantic-test-vector-search-cosine ()
  "Vector mode ranks by cosine; the query embedding is mocked (no network)."
  (anvil-semantic-test--with-corpus
    (let* ((db (anvil-semantic--ensure-db))
           (model (anvil-semantic--embed-model))
           (rows (sqlite-select db "SELECT digest, file, text FROM chunk_fts"))
           ;; target = the plain-text chunk containing "leakage"
           (target (cl-find-if (lambda (r) (string-search "leakage" (nth 2 r)))
                               rows)))
      (should target)
      ;; target gets [1,0]; every other chunk gets [0,1]
      (dolist (r rows)
        (anvil-semantic-test--put-vec
         db (nth 0 r) (nth 1 r) model
         (anvil-semantic--normalize
          (if (equal (nth 0 r) (nth 0 target)) (vector 1.0 0.0)
            (vector 0.0 1.0)))))
      (should (anvil-semantic--embed-available-p db model))
      (cl-letf (((symbol-function 'anvil-semantic--query-vec)
                 (lambda (_q) (anvil-semantic--normalize (vector 1.0 0.0)))))
        (let ((hits (anvil-semantic--search "anything" 5 nil "vector" nil)))
          (should hits)
          (should (plist-get (car hits) :cos))
          (should (equal (nth 1 target) (plist-get (car hits) :file)))
          ;; the JSON result carries a cos field
          (let* ((out (anvil-semantic--tool-search "anything" "5" nil "vector"))
                 (arr (json-parse-string out :object-type 'alist)))
            (should (> (length arr) 0))
            (should (assq 'cos (aref arr 0)))))))))

(ert-deftest anvil-semantic-test-hybrid-fuses-cos-and-bm25 ()
  "Hybrid mode merges a cosine hit with the FTS candidates."
  (anvil-semantic-test--with-corpus
    (let* ((db (anvil-semantic--ensure-db))
           (model (anvil-semantic--embed-model))
           (rows (sqlite-select db "SELECT digest, file, text FROM chunk_fts")))
      (dolist (r rows)
        (anvil-semantic-test--put-vec
         db (nth 0 r) (nth 1 r) model
         (anvil-semantic--normalize (vector 1.0 0.0))))
      (cl-letf (((symbol-function 'anvil-semantic--query-vec)
                 (lambda (_q) (anvil-semantic--normalize (vector 1.0 0.0)))))
        (let ((hits (anvil-semantic--search "leakage detection" 8 nil "hybrid" nil)))
          (should hits)
          ;; at least one hit carries cosine, at least one carries bm25
          (should (cl-find-if (lambda (h) (plist-get h :cos)) hits))
          (should (cl-find-if (lambda (h) (plist-get h :bm25)) hits)))))))

(provide 'anvil-semantic-test)
;;; anvil-semantic-test.el ends here
