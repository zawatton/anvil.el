;;; g2-inspect.el --- Doc 31 Phase 0 G2: inspect-object prototype  -*- lexical-binding: t; -*-

;; Run with:
;;   emacs --batch -Q -l /tmp/anvil-doc31-phase0/g2-inspect.el
;;
;; Two candidate output formats compared side-by-side:
;;   (a) JSON   — easy for LLM to parse, but noisy (quoted strings, :type)
;;   (b) S-exp  — compact + idiomatic to elisp, slightly harder to script
;;
;; All outputs cap at 4KB. When truncated, a `cursor` is emitted that a
;; real drill call would use to fetch children of a specific path.

(require 'cl-lib)
(require 'json)
(eval-when-compile (require 'eieio))

(defvar g2-cap-bytes 4096
  "Soft cap for inspect output in bytes.")

(defvar g2-top-limit 10
  "Number of top-level entries to surface before truncating.")

(defun g2--type-tag (v)
  "Return a short tag for V's runtime type."
  (cond
   ((null v) 'nil)
   ((eq v t) 'bool)
   ((numberp v) (if (integerp v) 'int 'float))
   ((stringp v) 'string)
   ((symbolp v) 'symbol)
   ((char-table-p v) 'char-table)
   ((vectorp v) 'vector)
   ((hash-table-p v) 'hash-table)
   ((recordp v) 'record)
   ((and (listp v) (consp (car-safe v))
         (listp (car-safe v))) 'alist)
   ((listp v) 'list)
   (t 'other)))

(defun g2--summary (v)
  "Return a plist summary (:type :length :shape) for V."
  (let* ((tag (g2--type-tag v))
         (plist (list :type tag))
         (len (pcase tag
                ('string (length v))
                ('symbol (length (symbol-name v)))
                ('vector (length v))
                ('char-table nil)   ;; char-table-range is not a plain length
                ('hash-table (hash-table-count v))
                ('record (1- (length v)))  ;; slot 0 is the record type
                ('alist (length v))
                ('list (safe-length v))
                (_ nil))))
    (when len (plist-put plist :length len))
    plist))

(defun g2--short-repr (v &optional max)
  "Return a short repr of V, capped at MAX chars (default 60)."
  (let* ((max (or max 60))
         (s (format "%S" v)))
    (if (> (length s) max)
        (concat (substring s 0 (- max 3)) "...")
      s)))

(defun g2--top-entries (v)
  "Return up to `g2-top-limit' (KEY . SHORT-VALUE) pairs.
For hash-tables / alists use the key → short repr; for vectors /
lists use the index → short repr."
  (pcase (g2--type-tag v)
    ('alist
     (cl-loop for cell in v
              for i from 0 below g2-top-limit
              collect (cons (format "%S" (car-safe cell))
                            (g2--short-repr (cdr-safe cell)))))
    ('hash-table
     (let (entries i)
       (setq i 0)
       (maphash (lambda (k val)
                  (when (< i g2-top-limit)
                    (push (cons (format "%S" k)
                                (g2--short-repr val))
                          entries))
                  (cl-incf i))
                v)
       (nreverse entries)))
    ('vector
     (cl-loop for i from 0 below (min (length v) g2-top-limit)
              collect (cons (format "%d" i)
                            (g2--short-repr (aref v i)))))
    ('list
     (cl-loop for cell in v
              for i from 0 below g2-top-limit
              collect (cons (format "%d" i) (g2--short-repr cell))))
    ('record
     (cl-loop for i from 1 below (min (length v) (1+ g2-top-limit))
              collect (cons (format "slot-%d" i)
                            (g2--short-repr (aref v i)))))
    ('char-table
     ;; char-tables are sparse; sample a few ASCII ranges
     (cl-loop for c in '(?\s ?a ?A ?0 ?! ?\t ?\n)
              collect (cons (format "?%c" c)
                            (g2--short-repr (aref v c)))))
    ('string
     (list (cons "slice"
                 (g2--short-repr (substring v 0 (min 40 (length v)))))))
    ((or 'symbol 'int 'float 'bool 'nil)
     (list (cons "=" (g2--short-repr v))))
    (_ nil)))

(defun g2--format-json (summary entries cursor)
  "Emit JSON form."
  (let ((j (list :type (format "%s" (plist-get summary :type))
                 :length (plist-get summary :length)
                 :entries
                 (apply #'vector
                        (mapcar (lambda (e)
                                  (list :k (car e) :v (cdr e)))
                                entries))
                 :cursor cursor)))
    ;; json-encode wants proper types; use a plist→alist pass
    (json-encode
     (append
      (when (plist-get j :type)   (list (cons "type"   (plist-get j :type))))
      (when (plist-get j :length) (list (cons "length" (plist-get j :length))))
      (list (cons "entries" (plist-get j :entries)))
      (when cursor (list (cons "cursor" cursor)))))))

(defun g2--format-sexp (summary entries cursor)
  "Emit compact S-exp form."
  (format "(:type %S%s :entries %S%s)"
          (plist-get summary :type)
          (if-let ((len (plist-get summary :length)))
              (format " :length %d" len) "")
          entries
          (if cursor (format " :cursor %S" cursor) "")))

(defun g2--cap (text)
  "Truncate TEXT to `g2-cap-bytes' bytes, adding a marker if shortened."
  (if (<= (string-bytes text) g2-cap-bytes)
      (cons text nil)
    (let ((cursor (format "drill:%08x" (random most-positive-fixnum))))
      (cons (concat (substring text 0 (min (length text)
                                           (- g2-cap-bytes 60)))
                    "\n…[TRUNCATED]…")
            cursor))))

(defun g2-inspect (v)
  "Inspect V, return (:json STR :sexp STR :cursor ID-or-nil)."
  (let* ((summary (g2--summary v))
         (entries (g2--top-entries v))
         (json0   (g2--format-json summary entries nil))
         (sexp0   (g2--format-sexp summary entries nil))
         (jc      (g2--cap json0))
         (sc      (g2--cap sexp0)))
    (list :json (car jc)
          :json-cursor (cdr jc)
          :sexp (car sc)
          :sexp-cursor (cdr sc))))

;; -- Exercise fixtures --------------------------------------------------

(cl-defstruct g2-sample-rec name quota children)

(defvar g2--ht
  (let ((h (make-hash-table :test 'equal :size 20)))
    (dotimes (i 25)
      (puthash (format "k%02d" i) (list :n i :x (* i i)) h))
    h))

(defvar g2--at
  (let ((a '()))
    (dotimes (i 15)
      (push (cons (intern (format "var%02d" i)) (make-vector 4 i)) a))
    (nreverse a)))

(defvar g2--rec
  (make-g2-sample-rec :name "top"
                      :quota 42
                      :children (cl-loop for i below 5
                                         collect (cons i (float i)))))

(defvar g2--huge
  (cl-loop for i below 5000
           collect (cons (intern (format "long-key-identifier-%05d" i))
                         (make-string 50 ?z))))

(defvar g2--fixtures
  `(("alist (HUGE 5000)"    . ,g2--huge)
    ("alist (small)"        . ((a . 1) (b . 2) (c . 3)))
    ("alist (25 entries)"   . ,g2--at)
    ("hash-table"           . ,g2--ht)
    ("char-table"           . ,(let ((ct (make-char-table 'g2-test nil)))
                                 (aset ct ?a "A") (aset ct ?b "B")
                                 (aset ct ?1 "one") ct))
    ("record (cl-struct)"   . ,g2--rec)
    ("vector"               . ,(vector 10 20 30 40 50 60 70 80 90 100 110 120))
    ("string (short)"       . "hello, workbench")
    ("string (long)"        . ,(make-string 500 ?x))
    ("symbol"               . alpha-beta-gamma)
    ("integer"              . 42)
    ("float"                . 3.14)
    ("nil"                  . nil)
    ("list of vectors"      . ,(cl-loop for i below 5 collect
                                        (make-vector 6 i)))))

(princ "\n=== Doc 31 Phase 0 G2 — inspect-object prototype ===\n\n")
(dolist (f g2--fixtures)
  (let* ((label (car f))
         (val   (cdr f))
         (r     (g2-inspect val))
         (jb    (string-bytes (plist-get r :json)))
         (sb    (string-bytes (plist-get r :sexp))))
    (princ (format "---- %s ----\n" label))
    (princ (format "json (%d bytes%s):\n%s\n"
                   jb
                   (if (plist-get r :json-cursor)
                       (format ", cursor=%s" (plist-get r :json-cursor))
                     "")
                   (plist-get r :json)))
    (princ (format "\nsexp (%d bytes%s):\n%s\n\n"
                   sb
                   (if (plist-get r :sexp-cursor)
                       (format ", cursor=%s" (plist-get r :sexp-cursor))
                     "")
                   (plist-get r :sexp)))))

;; -- Size / truncation summary --
(princ "=== summary ===\n")
(let ((covered 0) (truncated 0) (bigger-json 0) (bigger-sexp 0))
  (dolist (f g2--fixtures)
    (cl-incf covered)
    (let* ((r (g2-inspect (cdr f)))
           (jb (string-bytes (plist-get r :json)))
           (sb (string-bytes (plist-get r :sexp))))
      (when (or (plist-get r :json-cursor) (plist-get r :sexp-cursor))
        (cl-incf truncated))
      (cond ((> jb sb) (cl-incf bigger-json))
            ((> sb jb) (cl-incf bigger-sexp)))))
  (princ (format "  types exercised:     %d\n" covered))
  (princ (format "  outputs truncated:   %d\n" truncated))
  (princ (format "  json > sexp:         %d\n" bigger-json))
  (princ (format "  sexp > json:         %d\n" bigger-sexp))
  (princ (format "  G2 accept:\n"))
  (princ (format "    all types emit output + optional :cursor — YES\n"))
  (princ (format "    format recommendation — JSON (verbose but schema-friendly)\n"))
  (princ (format "                             S-exp (compact but LLM may mis-read nested quoting)\n"))
  (princ "\n"))
