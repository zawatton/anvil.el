;;; anvil-data.el --- JSON and CSV data tools for anvil -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Author: zawatton
;; Keywords: tools, mcp, claude, json, csv, data

;;; Commentary:

;; anvil-helpers の姉妹ファイル。Claude Code が emacs-eval 経由で
;; JSON 等のデータファイルを 1 call で型安全に読み書きするための helper。
;;
;; 主要動機:
;;   settings.json / report-plan.json / package-lock.json などを
;;   Edit + 手動 parse でなく、構造化された plist で扱う。
;;
;; 特徴:
;;   - UTF-8 強制 (`anvil--insert-file' で raw byte 罠回避)
;;   - 巨大ファイルでも with-temp-buffer + write-region で安全
;;   - JSON null/false は Emacs の :null / :false シンボルで往復可能
;;
;; 注意 (null/false handling):
;;   `json-parse-buffer' のデフォルトでは JSON null → :null シンボル、
;;   false → :false シンボルが返る。Lisp の `nil' は使われない。
;;   この helper はそれを引き継ぐ — 読み書きで往復しても sentinel が
;;   保たれる。Lisp の `nil' を意図的に渡したい場合は読み込み後に
;;   自分で変換すること。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'anvil-file)
(require 'anvil-server)

;;;; --- internal -----------------------------------------------------------

(defun anvil-data--require-parent (path)
  "Error if the parent directory of PATH does not exist."
  (let ((parent (file-name-directory (expand-file-name path))))
    (unless (and parent (file-directory-p parent))
      (error "anvil-data: parent directory does not exist: %s" parent))))

;;;; --- public API ---------------------------------------------------------

(defun anvil-data-read-json (path)
  "Read JSON from PATH and return it as a plist (objects) / vector (arrays).

JSON null is returned as the symbol `:null'; false as `:false'.
Numbers, strings, and `t' are returned as themselves. Use these
sentinels back when calling `anvil-data-write-json' for round-trip
fidelity.

Errors with `(error \"anvil-data: ...\")' on file or parse failure."
  (let ((abs (anvil--prepare-path path)))
    (with-temp-buffer
      (anvil--insert-file abs)
      (goto-char (point-min))
      (condition-case err
          (json-parse-buffer :object-type 'plist
                             :array-type 'array
                             :null-object :null
                             :false-object :false)
        (error
         (error "anvil-data: JSON parse error in %s: %s"
                abs (error-message-string err)))))))

(defun anvil-data-write-json (path data &optional pretty)
  "Write DATA to PATH as JSON (UTF-8, LF newlines).

PRETTY non-nil pretty-prints via `json-pretty-print-buffer'.

Round-trip with `anvil-data-read-json' is preserved when DATA uses
the `:null' / `:false' sentinels for JSON null / false; plain Lisp
`nil' inside DATA may be encoded as an empty array / object
depending on context (Emacs `json-serialize' semantics).

Returns (:file PATH :bytes N :pretty BOOL)."
  (anvil-data--require-parent path)
  (let ((abs (expand-file-name path)) bytes)
    (with-temp-buffer
      (insert (json-serialize data
                              :null-object :null
                              :false-object :false))
      (when pretty
        (json-pretty-print-buffer))
      (goto-char (point-max))
      (unless (eq (char-before) ?\n) (insert "\n"))
      (setq bytes (buffer-size))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) abs nil 'silent)))
    (list :file abs :bytes bytes :pretty (and pretty t))))

;;;; --- CSV ----------------------------------------------------------------

(defun anvil-data--parse-csv-line (line delim)
  "Parse a single CSV LINE into a list of strings.
Handles double-quoted fields including escaped quotes (\"\")."
  (let ((pos 0)
        (len (length line))
        (fields '())
        (current "")
        (in-quote nil))
    (while (< pos len)
      (let ((ch (aref line pos)))
        (cond
         (in-quote
          (cond
           ((and (= ch ?\")
                 (< (1+ pos) len)
                 (= (aref line (1+ pos)) ?\"))
            ;; Escaped quote
            (setq current (concat current "\""))
            (setq pos (+ pos 2)))
           ((= ch ?\")
            (setq in-quote nil)
            (setq pos (1+ pos)))
           (t
            (setq current (concat current (string ch)))
            (setq pos (1+ pos)))))
         (t
          (cond
           ((= ch ?\")
            (setq in-quote t)
            (setq pos (1+ pos)))
           ((= ch delim)
            (push current fields)
            (setq current "")
            (setq pos (1+ pos)))
           (t
            (setq current (concat current (string ch)))
            (setq pos (1+ pos))))))))
    (push current fields)
    (nreverse fields)))

(defun anvil-data-read-csv (path &optional opts)
  "Read CSV file PATH into a list of rows (each row a list of strings).

OPTS plist:
  :delimiter CHAR    field separator (default ?, comma)
  :encoding SYM      coding system for reading (default 'utf-8)
  :header BOOL       if t, return (:header (..) :rows ((..) ..)) instead
  :skip-rows N       skip N rows from the top before parsing
  :max-rows N        cap returned rows (default nil = all)

Quoted fields with escaped quotes (\"\") are supported. Newlines
inside quoted fields are NOT supported (rare and adds complexity).

Returns either:
  ((row1-field1 row1-field2 ..) (row2-field1 ..) ..)   ; default
  (:header (..) :rows ((..) ..))                        ; with :header t

For Money Forward / 会計ソフト の Shift_JIS CSV, pass :encoding 'cp932."
  (let* ((abs (anvil--prepare-path path))
         (delim (or (plist-get opts :delimiter) ?,))
         (encoding (or (plist-get opts :encoding) 'utf-8))
         (header-p (plist-get opts :header))
         (skip (or (plist-get opts :skip-rows) 0))
         (max-rows (plist-get opts :max-rows))
         (lines '()))
    (with-temp-buffer
      (let ((coding-system-for-read encoding))
        (insert-file-contents abs))
      (goto-char (point-min))
      ;; Skip leading rows
      (dotimes (_ skip)
        (forward-line 1))
      ;; Read remaining lines
      (while (and (not (eobp))
                  (or (null max-rows) (< (length lines) max-rows)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-empty-p line)
            (push (anvil-data--parse-csv-line line delim) lines))
          (forward-line 1))))
    (let ((rows (nreverse lines)))
      (if header-p
          (list :header (car rows) :rows (cdr rows))
        rows))))

;;;; --- Doc 33 Phase 1: JSON path-based edits ------------------------------
;;
;; Path syntax (single grammar across formats):
;;   segment := IDENT | "[" INDEX "]"
;;   IDENT   := [A-Za-z_][A-Za-z0-9_-]*
;;   INDEX   := 0 | [1-9][0-9]*
;;   path    := segment ( ("." segment) | ("[" INDEX "]") )*
;;
;; Empty path / "" refers to the root document.  See
;; docs/design/33-data-path.org for the design rationale.

(defconst anvil-data--server-id "emacs-eval"
  "Server id under which data-* MCP tools register.")

(defconst anvil-data--path-ident-re
  "[A-Za-z_][A-Za-z0-9_-]*"
  "Regexp matching a single dotted-path identifier segment.")

(defun anvil-data--parse-path (path)
  "Parse PATH (string) into a list of segments.
Each segment is one of:
  (:object KEYWORD)   — for `foo' style identifier access
  (:array INDEX)      — for `[0]' style index access
The empty / nil PATH returns nil (refers to the root document)."
  (cond
   ((null path) nil)
   ((not (stringp path))
    (error "anvil-data: path must be a string, got %S" path))
   ((string-empty-p path) nil)
   (t
    (let ((s path)
          out)
      (while (not (string-empty-p s))
        (cond
         ((string-prefix-p "[" s)
          (unless (string-match "\\`\\[\\([0-9]+\\)\\]" s)
            (error "anvil-data: malformed array index in path: %S" s))
          (push (list :array (string-to-number (match-string 1 s))) out)
          (setq s (substring s (match-end 0))))
         ((string-prefix-p "." s)
          (setq s (substring s 1))
          (when (string-empty-p s)
            (error "anvil-data: trailing `.' in path"))
          (when (or (string-prefix-p "." s) (string-prefix-p "[" s))
            (error "anvil-data: `.' must be followed by an identifier: %S" path)))
         ((string-match (concat "\\`\\(" anvil-data--path-ident-re "\\)") s)
          (push (list :object (intern (concat ":" (match-string 1 s)))) out)
          (setq s (substring s (match-end 0))))
         (t
          (error "anvil-data: cannot parse path remainder: %S" s))))
      (nreverse out)))))

(defun anvil-data--get-at (tree segments)
  "Walk TREE following SEGMENTS and return the value at the path.
Returns the sentinel `:anvil-data/missing' when any intermediate is
absent or has the wrong shape (e.g. trying to access a key on a
number).  Read-side traversal is lenient by design — a missing
path is not an error.  Set / delete are strict about shape because
a wrong-type assignment would corrupt the tree."
  (if (null segments)
      tree
    (let* ((seg (car segments))
           (kind (car seg))
           (key (cadr seg)))
      (pcase kind
        (:object
         (cond
          ((not (anvil-data--plist-p tree)) :anvil-data/missing)
          (t
           (let ((rest (plist-member tree key)))
             (if rest
                 (anvil-data--get-at (cadr rest) (cdr segments))
               :anvil-data/missing)))))
        (:array
         (cond
          ((not (vectorp tree)) :anvil-data/missing)
          ((or (< key 0) (>= key (length tree)))
           :anvil-data/missing)
          (t (anvil-data--get-at (aref tree key) (cdr segments)))))))))

(defun anvil-data--plist-p (x)
  "Return non-nil when X looks like a JSON-object plist (keyword keys)."
  (and (listp x)
       (or (null x)
           (keywordp (car x)))))

(defun anvil-data--set-at (tree segments value)
  "Return a new TREE with VALUE installed at SEGMENTS.
Creates intermediate maps / arrays as needed.  Errors when an existing
intermediate has the wrong type for the segment kind, or when an array
index is out of range (current length is OK = append)."
  (if (null segments)
      value
    (let* ((seg (car segments))
           (rest (cdr segments))
           (kind (car seg))
           (key (cadr seg)))
      (pcase kind
        (:object
         (let ((base (cond
                      ((or (null tree) (eq tree :anvil-data/missing)) nil)
                      ((anvil-data--plist-p tree) tree)
                      (t (error "anvil-data: cannot set object key on non-object %S"
                                tree)))))
           (anvil-data--plist-put base key
                                  (anvil-data--set-at
                                   (anvil-data--plist-get-or-missing base key)
                                   rest value))))
        (:array
         (let ((base (cond
                      ((or (null tree) (eq tree :anvil-data/missing)) (vector))
                      ((vectorp tree) tree)
                      (t (error "anvil-data: cannot set array index on non-array %S"
                                tree)))))
           (cond
            ((< key 0)
             (error "anvil-data: negative array index: %d" key))
            ((> key (length base))
             (error "anvil-data: array index %d out of range (length %d)"
                    key (length base)))
            ((= key (length base))
             ;; append
             (let ((appended (make-vector (1+ (length base)) nil)))
               (dotimes (i (length base))
                 (aset appended i (aref base i)))
               (aset appended key
                     (anvil-data--set-at :anvil-data/missing rest value))
               appended))
            (t
             (let ((copy (copy-sequence base)))
               (aset copy key
                     (anvil-data--set-at (aref base key) rest value))
               copy)))))))))

(defun anvil-data--plist-get-or-missing (plist key)
  "Return value for KEY in PLIST, or `:anvil-data/missing'."
  (let ((cell (plist-member plist key)))
    (if cell (cadr cell) :anvil-data/missing)))

(defun anvil-data--plist-put (plist key value)
  "Return a new plist with KEY set to VALUE (preserves order, appends new)."
  (if (plist-member plist key)
      (let ((out '()))
        (while plist
          (if (eq (car plist) key)
              (progn (push key out) (push value out))
            (push (car plist) out)
            (push (cadr plist) out))
          (setq plist (cddr plist)))
        (nreverse out))
    (append plist (list key value))))

(defun anvil-data--delete-at (tree segments)
  "Return a new TREE with SEGMENTS removed.
Returns the symbol `:anvil-data/missing' when the path didn't exist
in the original tree (caller turns this into a noop)."
  (cond
   ((null segments)
    ;; delete the root => empty
    nil)
   ((null (cdr segments))
    ;; final segment: actually remove
    (let* ((seg (car segments))
           (kind (car seg))
           (key (cadr seg)))
      (pcase kind
        (:object
         (cond
          ((not (anvil-data--plist-p tree)) :anvil-data/missing)
          ((not (plist-member tree key)) :anvil-data/missing)
          (t (anvil-data--plist-remove tree key))))
        (:array
         (cond
          ((not (vectorp tree)) :anvil-data/missing)
          ((or (< key 0) (>= key (length tree))) :anvil-data/missing)
          (t (anvil-data--vector-remove tree key)))))))
   (t
    ;; recurse
    (let* ((seg (car segments))
           (rest (cdr segments))
           (kind (car seg))
           (key (cadr seg)))
      (pcase kind
        (:object
         (cond
          ((not (anvil-data--plist-p tree)) :anvil-data/missing)
          ((not (plist-member tree key)) :anvil-data/missing)
          (t
           (let ((sub (anvil-data--delete-at (plist-get tree key) rest)))
             (if (eq sub :anvil-data/missing)
                 :anvil-data/missing
               (anvil-data--plist-put tree key sub))))))
        (:array
         (cond
          ((not (vectorp tree)) :anvil-data/missing)
          ((or (< key 0) (>= key (length tree))) :anvil-data/missing)
          (t
           (let ((sub (anvil-data--delete-at (aref tree key) rest)))
             (if (eq sub :anvil-data/missing)
                 :anvil-data/missing
               (let ((copy (copy-sequence tree)))
                 (aset copy key sub)
                 copy)))))))))))

(defun anvil-data--plist-remove (plist key)
  "Return a new plist without KEY."
  (let (out)
    (while plist
      (unless (eq (car plist) key)
        (push (car plist) out)
        (push (cadr plist) out))
      (setq plist (cddr plist)))
    (nreverse out)))

(defun anvil-data--vector-remove (vec idx)
  "Return a new vector with element at IDX removed."
  (let ((out (make-vector (1- (length vec)) nil))
        (j 0))
    (dotimes (i (length vec))
      (unless (= i idx)
        (aset out j (aref vec i))
        (cl-incf j)))
    out))

(defun anvil-data--keys-at (tree)
  "Return the keys of the map at TREE (or array indices).
Plist objects: keys (without leading colon) as strings.
Vectors: 0..N-1 as strings.
Other: error."
  (cond
   ((vectorp tree)
    (let (out)
      (dotimes (i (length tree))
        (push (number-to-string i) out))
      (nreverse out)))
   ((anvil-data--plist-p tree)
    (let (out (p tree))
      (while p
        (push (substring (symbol-name (car p)) 1) out)
        (setq p (cddr p)))
      (nreverse out)))
   (t
    (error "anvil-data: cannot list keys of non-collection: %S" tree))))

(defun anvil-data--detect-format (path)
  "Detect data format from PATH file extension; default `json'."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond
     ((member ext '("json" "json5" "jsonc")) 'json)
     ((member ext '("yaml" "yml")) 'yaml)
     ((member ext '("toml")) 'toml)
     (t 'json))))

(defun anvil-data--require-supported-format (fmt)
  "Error when FMT is not yet implemented (Phase 1 ships JSON only)."
  (unless (eq fmt 'json)
    (error "anvil-data: format %S not yet implemented (Doc 33 Phase 1 = JSON only)"
           fmt)))

(defun anvil-data--read-tree (path fmt)
  "Read PATH (already absolute) into a Lisp tree per FMT."
  (anvil-data--require-supported-format fmt)
  (anvil-data-read-json path))

(defun anvil-data--render (tree fmt)
  "Render TREE back to a string per FMT, with sane formatting."
  (anvil-data--require-supported-format fmt)
  (let ((raw (json-serialize tree
                             :null-object :null
                             :false-object :false)))
    (with-temp-buffer
      (insert raw)
      (goto-char (point-min))
      (json-pretty-print-buffer)
      (goto-char (point-max))
      (unless (eq (char-before) ?\n) (insert "\n"))
      (buffer-string))))

(defun anvil-data--write-content (path content)
  "Write CONTENT to PATH as UTF-8 with LF newlines."
  (anvil-data--require-parent path)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path nil 'silent))))

(cl-defun anvil-data-path-get (file path &key format)
  "Return the value at PATH inside FILE.
Returns nil when the path does not exist (use `data-list-keys' to
distinguish missing-key from explicit nil/null).  FORMAT defaults to
the file extension; only JSON is supported in Phase 1."
  (let* ((abs (anvil--prepare-path file))
         (fmt (or format (anvil-data--detect-format abs)))
         (tree (anvil-data--read-tree abs fmt))
         (segments (anvil-data--parse-path path))
         (val (anvil-data--get-at tree segments)))
    (if (eq val :anvil-data/missing) nil val)))

(cl-defun anvil-data-path-set (file path value &key apply format)
  "Install VALUE at PATH inside FILE.
VALUE may be any Lisp object compatible with `json-serialize' or the
sentinel `:null' / `:false'.  When APPLY is non-nil the file is
rewritten; when nil only the preview content is built and returned.
Returns plist (:file :path :before :after :applied :preview)."
  (let* ((abs (anvil--prepare-path file))
         (fmt (or format (anvil-data--detect-format abs)))
         (tree (anvil-data--read-tree abs fmt))
         (segments (anvil-data--parse-path path))
         (before (anvil-data--render tree fmt))
         (new-tree (anvil-data--set-at tree segments value))
         (after (anvil-data--render new-tree fmt)))
    (when apply
      (anvil-data--write-content abs after))
    (list :file abs :path path
          :before-bytes (length before)
          :after-bytes (length after)
          :applied (and apply t)
          :preview after)))

(cl-defun anvil-data-path-delete (file path &key apply format)
  "Remove PATH inside FILE.
Same preview / apply contract as `anvil-data-path-set'.  Returns plist
with `:noop t' when the path was already absent."
  (let* ((abs (anvil--prepare-path file))
         (fmt (or format (anvil-data--detect-format abs)))
         (tree (anvil-data--read-tree abs fmt))
         (segments (anvil-data--parse-path path))
         (new-tree (anvil-data--delete-at tree segments))
         (noop (eq new-tree :anvil-data/missing))
         (final (if noop tree new-tree))
         (after (anvil-data--render final fmt)))
    (when (and apply (not noop))
      (anvil-data--write-content abs after))
    (list :file abs :path path
          :noop noop
          :applied (and apply (not noop) t)
          :preview after)))

(cl-defun anvil-data-path-keys (file &optional path &key format)
  "Return the keys of the map / array at PATH inside FILE.
PATH nil / empty refers to the root.  Object keys come back without
the leading colon; array indices come back as numeric strings."
  (let* ((abs (anvil--prepare-path file))
         (fmt (or format (anvil-data--detect-format abs)))
         (tree (anvil-data--read-tree abs fmt))
         (segments (anvil-data--parse-path path))
         (sub (anvil-data--get-at tree segments)))
    (when (eq sub :anvil-data/missing)
      (error "anvil-data: path not found: %S" path))
    (anvil-data--keys-at sub)))


;;;; --- Doc 33 MCP handlers ------------------------------------------------

(defun anvil-data--coerce-string (s)
  (and (stringp s) (not (string-empty-p s)) s))

(defun anvil-data--coerce-bool (s)
  (and (stringp s)
       (not (member s '("" "nil" "false" "0" "no" "False" "NIL")))))

(defun anvil-data--decode-value-json (value-json)
  "Parse VALUE-JSON into the same plist / vector tree shape as `data-read-json'."
  (when (or (null value-json) (string-empty-p value-json))
    (error "anvil-data: value-json is required"))
  (json-parse-string value-json
                     :object-type 'plist
                     :array-type 'array
                     :null-object :null
                     :false-object :false))

(defun anvil-data--tool-get-path (file path)
  "MCP wrapper for `anvil-data-path-get'.

MCP Parameters:
  file - Absolute or relative path to the JSON file.
  path - Dotted path (e.g. `mcpServers.emacs-eval.command'); empty
         string returns the full document."
  (anvil-server-with-error-handling
    (let ((value (anvil-data-path-get file (or path ""))))
      (list :file (expand-file-name file)
            :path (or path "")
            :value value))))

(defun anvil-data--tool-set-path (file path value-json &optional apply)
  "MCP wrapper for `anvil-data-path-set'.

MCP Parameters:
  file       - Path to the JSON file.
  path       - Dotted path to install at.
  value-json - JSON-encoded string for the new value (e.g. `\"42\"',
               `\"\\\"x\\\"\"', `\"[1,2,3]\"', `\"{\\\"k\\\":\\\"v\\\"}\"').
  apply      - Optional truthy string to actually write the file
               (preview-only by default)."
  (anvil-server-with-error-handling
    (anvil-data-path-set file (or path "")
                         (anvil-data--decode-value-json value-json)
                         :apply (anvil-data--coerce-bool apply))))

(defun anvil-data--tool-delete-path (file path &optional apply)
  "MCP wrapper for `anvil-data-path-delete'.

MCP Parameters:
  file  - Path to the JSON file.
  path  - Dotted path to remove.
  apply - Optional truthy string to actually write."
  (anvil-server-with-error-handling
    (anvil-data-path-delete file (or path "")
                            :apply (anvil-data--coerce-bool apply))))

(defun anvil-data--tool-list-keys (file &optional path)
  "MCP wrapper for `anvil-data-path-keys'.

MCP Parameters:
  file - Path to the JSON file.
  path - Optional dotted path (defaults to the root)."
  (anvil-server-with-error-handling
    (let ((keys (anvil-data-path-keys file (or path ""))))
      (list :file (expand-file-name file)
            :path (or path "")
            :keys keys))))


;;;; --- Doc 33 lifecycle ---------------------------------------------------

(defun anvil-data--register-tools ()
  "Register the Doc 33 Phase 1 path-* MCP tools."
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-data--tool-get-path)
   :id "data-get-path"
   :intent '(json-edit config file-read)
   :layer 'core
   :server-id anvil-data--server-id
   :description
   "Read the value at a dotted path inside a JSON file.  Returns
\(:file :path :value) — VALUE is the parsed Lisp tree (plist for
objects, vector for arrays, sentinel `:null' / `:false' for those
JSON literals).  Empty PATH returns the whole document.  Read-only."
   :read-only t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-data--tool-set-path)
   :id "data-set-path"
   :intent '(json-edit config)
   :layer 'core
   :server-id anvil-data--server-id
   :description
   "Install a JSON value at a dotted path inside a JSON file.
VALUE-JSON is a string the caller has serialised with `json-serialize'
(or by hand) so the MCP boundary stays unambiguous about types.
APPLY is preview-only by default — pass any truthy string (e.g.
\"t\") to actually rewrite the file via UTF-8 + LF.  Returns
\(:file :path :before-bytes :after-bytes :applied :preview)."
   :destructive t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-data--tool-delete-path)
   :id "data-delete-path"
   :intent '(json-edit config)
   :layer 'core
   :server-id anvil-data--server-id
   :description
   "Remove a dotted path from a JSON file.  Same preview / apply
contract as `data-set-path'.  Returns plist with `:noop t' when the
path was already absent."
   :destructive t)
  (anvil-server-register-tool
   (anvil-server-encode-handler #'anvil-data--tool-list-keys)
   :id "data-list-keys"
   :intent '(json-edit file-read)
   :layer 'core
   :server-id anvil-data--server-id
   :description
   "Return the keys (or array indices) of the map / array at the
named path.  Object keys come back without their leading colon;
array indices come back as numeric strings.  Read-only."
   :read-only t))

(defun anvil-data--unregister-tools ()
  (dolist (id '("data-get-path" "data-set-path"
                "data-delete-path" "data-list-keys"))
    (ignore-errors
      (anvil-server-unregister-tool id anvil-data--server-id))))

;;;###autoload
(defun anvil-data-enable ()
  "Register the Doc 33 Phase 1 data-* MCP tools."
  (interactive)
  (anvil-data--register-tools))

;;;###autoload
(defun anvil-data-disable ()
  "Unregister the Doc 33 Phase 1 data-* MCP tools."
  (interactive)
  (anvil-data--unregister-tools))


;;;; --- discoverability ----------------------------------------------------

(defun anvil-data-helpers-list ()
  "Return a list of all anvil-data* user-facing function names."
  (let (results)
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (let ((n (symbol-name sym)))
                    (and (string-prefix-p "anvil-data" n)
                         (not (string-match-p "--" n)))))
         (push sym results))))
    (sort results (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(provide 'anvil-data)
;;; anvil-data.el ends here
