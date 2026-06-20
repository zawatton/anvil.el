;;; anvil-cad.el --- CAD file (DXF/SVG) read/edit via a format-agnostic entity IR -*- lexical-binding: t; -*-
;;; anvil-audit: tools-wrapped-at-registration

;; Copyright (C) 2025-2026 zawatton

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Read and (in later phases) edit CAD drawings through one
;; format-agnostic intermediate representation (IR).  A CAD file is
;; parsed into a list of *entity* plists; analysis and editing operate
;; on that IR, and each format gets only a thin parser/writer adapter.
;;
;; Phase 1: DXF read path.
;;   - `anvil-cad--dxf-parse'  : ASCII DXF text -> (:format dxf
;;                               :sections .. :entities .. :layers ..)
;;   - MCP tool `cad-read-outline' : structural summary (sections,
;;                               layers, entity-type census, bbox)
;;   - MCP tool `cad-extract'      : entities as structured IR, with
;;                               optional layer/type filters
;;
;; Phase 2: DXF write path.  The lossless :pairs slot of each entity is
;; the source of truth; editing rewrites pairs and only the ENTITIES
;; section is regenerated (`anvil-cad--render-dxf' splices it between the
;; verbatim PRE/POST pair regions), so HEADER/TABLES/BLOCKS survive.
;;   - MCP tool `cad-annotate'     : add a TEXT label at a point/layer
;;   - MCP tool `cad-batch-update'  : literal find/replace on text and/or
;;                               relayer, scoped by layer/type/match filters
;;   - MCP tool `cad-generate'      : build line/text/circle entities from
;;                               a JSON spec, standalone or appended to a base
;;
;; Phase 3: SVG, on the same IR and the same 5 tools.  Since NeLisp has no
;; libxml, a small pure-Elisp XML parser/serializer (`anvil-cad--xml-*')
;; backs it; the DOM is the source of truth for SVG writes the way :pairs
;; is for DXF.  Read maps DOM->IR (`anvil-cad--svg-parse'); write builds or
;; edits the DOM and re-serializes.  SVG `layer' maps to the class
;; attribute (and Inkscape layer groups on read).  Format is chosen by
;; file extension throughout (.dxf / .svg).
;;
;; Design note — dual-target.  The DXF parser core (`anvil-cad--dxf-*',
;; `anvil-cad--num', `anvil-cad--entity-from-pairs', `anvil-cad--outline',
;; `anvil-cad--filter-entities', `anvil-cad--distinct-layers') is a PURE
;; function of its input string and uses only portable Elisp primitives
;; (split-string / string-trim / string-to-number / replace-regexp-in-string).
;; It therefore runs identically on Emacs and on the NeLisp runtime.
;; Only the file slurp and the MCP handlers depend on the host.
;;
;; IR entity plist:
;;   (:type SYMBOL        ; entity kind, downcased DXF type (line/text/circle..)
;;    :layer STRING       ; group code 8 (omitted when absent)
;;    :text STRING        ; group codes 1/3 joined (TEXT/MTEXT/ATTRIB content)
;;    :p1 (X Y Z)         ; first 10/20/30 point (Z defaults 0.0)
;;    :p2 (X Y Z)         ; first 11/21/31 point
;;    :vertices N         ; count of code-10 occurrences when > 1 (polylines)
;;    :pairs ((CODE . VALUE) ...))  ; lossless ordered group pairs (for Phase 2 write)
;;
;; Scope: ASCII DXF and SVG (binary DXF and DWG are out of scope).

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-cad nil
  "CAD file (DXF/SVG) read/edit via a format-agnostic entity IR."
  :group 'anvil
  :prefix "anvil-cad-")

(defconst anvil-cad--server-id "emacs-eval"
  "Server ID under which anvil-cad MCP tools are registered.")

(defcustom anvil-cad-max-entities 2000
  "Maximum number of entities returned by `cad-extract'.
Results with more entities are truncated and :truncated becomes t.
Prevents accidental token blowups on large drawings."
  :type 'integer
  :group 'anvil-cad)

;;;; --- DXF parsing core (pure, dual-target) -------------------------------

(defun anvil-cad--normalize-newlines (text)
  "Convert CRLF and lone CR line endings in TEXT to LF."
  (replace-regexp-in-string "\r\n?" "\n" (or text "")))

(defun anvil-cad--num (s)
  "Parse DXF numeric string S into a number, tolerant of surrounding space."
  (string-to-number (string-trim (or s ""))))

(defun anvil-cad--dxf-pairs (text)
  "Parse DXF TEXT into an ordered list of (CODE . VALUE) cons cells.
CODE is the integer group code; VALUE is the raw value string.  DXF is
strictly line-paired (code line, then value line); a code line that is
not a run of digits is skipped together with its value so a stray blank
line cannot derail the walk."
  (let* ((lines (split-string (anvil-cad--normalize-newlines text) "\n"))
         (pairs nil))
    (while (and lines (cdr lines))
      (let ((code-line (string-trim (car lines)))
            (val-line (car (cdr lines))))
        (setq lines (cddr lines))
        (when (string-match-p "\\`[0-9]+\\'" code-line)
          (setq pairs (cons (cons (string-to-number code-line) val-line)
                            pairs)))))
    (nreverse pairs)))

(defun anvil-cad--entity-from-pairs (type pairs)
  "Build an IR entity plist from DXF entity TYPE string and its PAIRS.
PAIRS is the ordered list of (CODE . VALUE) following the 0/TYPE marker."
  (let ((layer nil) (text-chunks nil)
        (p1x nil) (p1y nil) (p1z nil)
        (p2x nil) (p2y nil) (p2z nil)
        (vtx 0))
    (dolist (pr pairs)
      (let ((c (car pr)) (v (cdr pr)))
        (cond
         ((= c 8) (setq layer v))
         ((or (= c 1) (= c 3)) (setq text-chunks (cons v text-chunks)))
         ((= c 10) (setq vtx (1+ vtx)) (unless p1x (setq p1x (anvil-cad--num v))))
         ((= c 20) (unless p1y (setq p1y (anvil-cad--num v))))
         ((= c 30) (unless p1z (setq p1z (anvil-cad--num v))))
         ((= c 11) (unless p2x (setq p2x (anvil-cad--num v))))
         ((= c 21) (unless p2y (setq p2y (anvil-cad--num v))))
         ((= c 31) (unless p2z (setq p2z (anvil-cad--num v)))))))
    (let ((p1 (and p1x (list p1x (or p1y 0.0) (or p1z 0.0))))
          (p2 (and p2x p2y (list p2x p2y (or p2z 0.0))))
          (text (and text-chunks
                     (mapconcat #'identity (nreverse text-chunks) ""))))
      (append
       (list :type (intern (downcase type)))
       (and layer (list :layer layer))
       (and text (list :text text))
       (and p1 (list :p1 p1))
       (and p2 (list :p2 p2))
       (and (> vtx 1) (list :vertices vtx))
       (list :pairs pairs)))))

(defun anvil-cad--distinct-layers (entities)
  "Return distinct :layer values across ENTITIES, in first-seen order."
  (let ((seen nil) (out nil))
    (dolist (e entities)
      (let ((l (plist-get e :layer)))
        (when (and l (not (member l seen)))
          (setq seen (cons l seen))
          (setq out (cons l out)))))
    (nreverse out)))

(defun anvil-cad--dxf-parse (text)
  "Parse ASCII DXF TEXT into a plist.
Returns (:format dxf :sections SECTION-NAMES :entities IR-LIST
:layers LAYER-NAMES).  Pure function — see file Commentary."
  (let ((pairs (anvil-cad--dxf-pairs text))
        (sections nil) (entities nil)
        (in-entities nil) (cur-section nil)
        (cur-type nil) (cur-pairs nil))
    (dolist (pr pairs)
      (let ((code (car pr)) (val (cdr pr)))
        (cond
         ((= code 0)
          ;; Flush an entity in progress before acting on the marker.
          (when (and in-entities cur-type)
            (setq entities
                  (cons (anvil-cad--entity-from-pairs cur-type (nreverse cur-pairs))
                        entities))
            (setq cur-type nil cur-pairs nil))
          (cond
           ((string= val "SECTION") (setq cur-section 'pending))
           ((string= val "ENDSEC") (setq in-entities nil cur-section nil))
           ((string= val "EOF") nil)
           (in-entities (setq cur-type val cur-pairs nil))
           (t nil)))
         ((and (= code 2) (eq cur-section 'pending))
          (setq sections (cons val sections))
          (setq cur-section val)
          (setq in-entities (string= val "ENTITIES")))
         ((and in-entities cur-type)
          (setq cur-pairs (cons pr cur-pairs))))))
    ;; Flush a trailing entity (file without a closing ENDSEC/EOF).
    (when (and cur-type cur-pairs)
      (setq entities
            (cons (anvil-cad--entity-from-pairs cur-type (nreverse cur-pairs))
                  entities)))
    (setq entities (nreverse entities))
    (list :format 'dxf
          :sections (nreverse sections)
          :entities entities
          :layers (anvil-cad--distinct-layers entities))))

;;;; --- analysis (pure) ----------------------------------------------------

(defun anvil-cad--outline (parsed)
  "Summarize PARSED (from `anvil-cad--dxf-parse') into an outline plist.
Returns (:format F :sections .. :layers .. :entity-count N
:entity-types ((TYPE . COUNT)..) :bbox (XMIN YMIN XMAX YMAX))."
  (let ((entities (plist-get parsed :entities))
        (types nil) (bbox nil))
    (dolist (e entities)
      (let* ((ty (plist-get e :type))
             (cell (assq ty types)))
        (if cell (setcdr cell (1+ (cdr cell)))
          (setq types (cons (cons ty 1) types))))
      (dolist (p (list (plist-get e :p1) (plist-get e :p2)))
        (when p
          (let ((x (nth 0 p)) (y (nth 1 p)))
            (if bbox
                (setq bbox (list (min (nth 0 bbox) x) (min (nth 1 bbox) y)
                                 (max (nth 2 bbox) x) (max (nth 3 bbox) y)))
              (setq bbox (list x y x y)))))))
    (setq types (sort types (lambda (a b) (> (cdr a) (cdr b)))))
    (list :format (plist-get parsed :format)
          :sections (plist-get parsed :sections)
          :layers (plist-get parsed :layers)
          :entity-count (length entities)
          :entity-types types
          :bbox bbox)))

(defun anvil-cad--filter-entities (entities layer type)
  "Filter ENTITIES by LAYER (string or nil) and TYPE (symbol or nil)."
  (let ((out nil))
    (dolist (e entities)
      (when (and (or (null layer) (equal (plist-get e :layer) layer))
                 (or (null type) (eq (plist-get e :type) type)))
        (setq out (cons e out))))
    (nreverse out)))

(defun anvil-cad--strip-pairs (e)
  "Return entity plist E without its bulky :pairs member.
Key/value order is preserved (pairs are kept intact, not flattened
through a member-wise `nreverse')."
  (let ((out nil) (rest e))
    (while rest
      (unless (eq (car rest) :pairs)
        (setq out (cons (list (car rest) (cadr rest)) out)))
      (setq rest (cddr rest)))
    (apply #'append (nreverse out))))

;;;; --- host-side file access ----------------------------------------------

(defun anvil-cad--detect-format (path)
  "Return a format symbol (dxf/svg) inferred from PATH extension, or nil."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond ((string= ext "dxf") 'dxf)
          ((string= ext "svg") 'svg)
          (t nil))))

(defun anvil-cad--slurp (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun anvil-cad-parse-file (path)
  "Read and parse the CAD file at PATH into IR.  Supports DXF and SVG."
  (let* ((abs (expand-file-name path))
         (fmt (anvil-cad--detect-format abs)))
    (unless (file-exists-p abs)
      (user-error "anvil-cad: %s does not exist" abs))
    (cond
     ((eq fmt 'dxf) (anvil-cad--dxf-parse (anvil-cad--slurp abs)))
     ((eq fmt 'svg) (anvil-cad--svg-parse (anvil-cad--slurp abs)))
     (t (user-error "anvil-cad: unsupported extension (expected .dxf/.svg): %s"
                    abs)))))

;;;; --- MCP tool handlers --------------------------------------------------

(defun anvil-cad--tool-read-outline (path)
  "Summarize the structure of a CAD drawing without dumping its entities.

MCP Parameters:
  path - Absolute path to a CAD file (string).  Phase 1 accepts .dxf
         (ASCII) only.

Returns a printed plist:
  (:format dxf :sections (NAME..) :layers (NAME..) :entity-count N
   :entity-types ((TYPE . COUNT)..) :bbox (XMIN YMIN XMAX YMAX))
TYPE is the lowercased DXF entity kind; the census is sorted by count
descending.  BBOX is the bounding box of all extracted points, or nil
when the drawing has none."
  (anvil-server-with-error-handling
   (format "%S" (anvil-cad--outline (anvil-cad-parse-file path)))))

(defun anvil-cad--tool-extract (path &optional layer type)
  "Extract drawing entities as structured IR, optionally filtered.

MCP Parameters:
  path  - Absolute path to a CAD file (string).  Phase 1 accepts .dxf
          (ASCII) only.
  layer - Optional layer name (string).  When non-empty, only entities
          on that layer are returned.  Leave empty for all layers.
  type  - Optional entity type (string, e.g. \"text\", \"line\",
          \"circle\").  Case-insensitive.  Leave empty for all types.

Returns a printed plist:
  (:format dxf :count N :truncated BOOL :entities (ENTITY..))
Each ENTITY is a plist (:type :layer :text :p1 :p2 :vertices); the raw
group :pairs are omitted to keep the payload compact.  N is the number
of entities returned (after filtering and truncation to
`anvil-cad-max-entities')."
  (anvil-server-with-error-handling
   (let* ((parsed (anvil-cad-parse-file path))
          (entities (plist-get parsed :entities))
          (ty (and type (not (string-empty-p (string-trim type)))
                   (intern (downcase (string-trim type)))))
          (lay (and layer (not (string-empty-p (string-trim layer)))
                    layer))
          (filtered (anvil-cad--filter-entities entities lay ty))
          (n (length filtered))
          (truncated (> n anvil-cad-max-entities))
          (kept (if truncated (cl-subseq filtered 0 anvil-cad-max-entities)
                  filtered)))
     (format "%S"
             (list :format (plist-get parsed :format)
                   :count (length kept)
                   :truncated (and truncated t)
                   :entities (mapcar #'anvil-cad--strip-pairs kept))))))

;;;; --- DXF writing core (pure, dual-target) -------------------------------

(defun anvil-cad--fmt-num (n)
  "Format number N for DXF output (reals always carry a decimal point).
Strings pass through unchanged so verbatim parsed values survive."
  (cond ((integerp n) (concat (number-to-string n) ".0"))
        ((floatp n) (number-to-string n))
        ((stringp n) n)
        (t (user-error "anvil-cad: not a number: %S" n))))

(defun anvil-cad--pairs-to-text (pairs)
  "Serialize a list of (CODE . VALUE) PAIRS back to DXF text.
Each pair becomes a code line and a value line.  Returns \"\" for nil."
  (if (null pairs) ""
    (concat (mapconcat (lambda (pr)
                         (concat (number-to-string (car pr)) "\n" (cdr pr)))
                       pairs "\n")
            "\n")))

(defun anvil-cad--entity-to-text (e)
  "Serialize IR entity E to DXF text: the 0/TYPE marker plus its :pairs."
  (anvil-cad--pairs-to-text
   (cons (cons 0 (upcase (symbol-name (plist-get e :type))))
         (plist-get e :pairs))))

(defun anvil-cad--entities-to-text (entities)
  "Serialize ENTITIES (a list of IR entities) to concatenated DXF text."
  (mapconcat #'anvil-cad--entity-to-text entities ""))

(defun anvil-cad--split-entities-region (text)
  "Return (PRE POST) pair-lists bracketing the ENTITIES section body, or nil.
PRE runs through the 2/ENTITIES pair; POST starts at the closing 0/ENDSEC.
Splicing new entities between PRE and POST preserves every other section
\(HEADER, TABLES, BLOCKS, OBJECTS ...) verbatim."
  (let ((pairs (anvil-cad--dxf-pairs text))
        (pre nil) (post nil) (state 'pre) (sec-pending nil))
    (dolist (pr pairs)
      (let ((c (car pr)) (v (cdr pr)))
        (cond
         ((eq state 'pre)
          (setq pre (cons pr pre))
          (cond
           ((and (= c 0) (string= v "SECTION")) (setq sec-pending t))
           ((and (= c 2) sec-pending)
            (setq sec-pending nil)
            (when (string= v "ENTITIES") (setq state 'skip)))))
         ((eq state 'skip)
          (when (and (= c 0) (string= v "ENDSEC"))
            (setq state 'post)
            (setq post (cons pr post))))
         ((eq state 'post)
          (setq post (cons pr post))))))
    (if (eq state 'pre) nil
      (list (nreverse pre) (nreverse post)))))

(defun anvil-cad--skeleton-dxf (entities)
  "Return a minimal standalone R12 DXF wrapping ENTITIES."
  (concat "0\nSECTION\n2\nHEADER\n9\n$ACADVER\n1\nAC1009\n0\nENDSEC\n"
          "0\nSECTION\n2\nENTITIES\n"
          (anvil-cad--entities-to-text entities)
          "0\nENDSEC\n0\nEOF\n"))

(defun anvil-cad--render-dxf (text new-entities)
  "Return DXF text equal to TEXT but with its ENTITIES body set to NEW-ENTITIES.
When TEXT has an ENTITIES section its body is replaced and all other
content is preserved verbatim.  When it has none, an ENTITIES section is
synthesized just before EOF, keeping the existing content."
  (let ((regions (anvil-cad--split-entities-region text)))
    (if regions
        (concat (anvil-cad--pairs-to-text (car regions))
                (anvil-cad--entities-to-text new-entities)
                (anvil-cad--pairs-to-text (cadr regions)))
      (let* ((pairs (anvil-cad--dxf-pairs text))
             (eofless nil))
        (dolist (pr pairs)
          (unless (and (= (car pr) 0) (string= (cdr pr) "EOF"))
            (setq eofless (cons pr eofless))))
        (setq eofless (nreverse eofless))
        (concat (anvil-cad--pairs-to-text eofless)
                "0\nSECTION\n2\nENTITIES\n"
                (anvil-cad--entities-to-text new-entities)
                "0\nENDSEC\n0\nEOF\n")))))

;;;; --- entity builders (pure) ---------------------------------------------

(defun anvil-cad--text-pairs (text p layer height)
  "Build R12 TEXT body pairs for TEXT at point P on LAYER with HEIGHT."
  (list (cons 8 layer)
        (cons 10 (anvil-cad--fmt-num (nth 0 p)))
        (cons 20 (anvil-cad--fmt-num (nth 1 p)))
        (cons 30 "0.0")
        (cons 40 (anvil-cad--fmt-num height))
        (cons 1 text)))

(defun anvil-cad--line-pairs (p1 p2 layer)
  "Build R12 LINE body pairs from P1 to P2 on LAYER."
  (list (cons 8 layer)
        (cons 10 (anvil-cad--fmt-num (nth 0 p1)))
        (cons 20 (anvil-cad--fmt-num (nth 1 p1)))
        (cons 30 "0.0")
        (cons 11 (anvil-cad--fmt-num (nth 0 p2)))
        (cons 21 (anvil-cad--fmt-num (nth 1 p2)))
        (cons 31 "0.0")))

(defun anvil-cad--circle-pairs (c r layer)
  "Build R12 CIRCLE body pairs centered at C with radius R on LAYER."
  (list (cons 8 layer)
        (cons 10 (anvil-cad--fmt-num (nth 0 c)))
        (cons 20 (anvil-cad--fmt-num (nth 1 c)))
        (cons 30 "0.0")
        (cons 40 (anvil-cad--fmt-num r))))

(defun anvil-cad--spec-to-entity (spec)
  "Build an IR entity from a generate SPEC alist (symbol keys).
SPEC keys: type (line/text/circle), layer, and per-type geometry —
line: p1 p2; text: p1|at, text, height; circle: center|at, radius."
  (let* ((type (downcase (or (alist-get 'type spec)
                             (user-error "anvil-cad: spec missing `type'"))))
         (layer (or (alist-get 'layer spec) "0")))
    (cond
     ((string= type "line")
      (anvil-cad--entity-from-pairs
       "LINE" (anvil-cad--line-pairs
               (or (alist-get 'p1 spec) (user-error "line needs p1"))
               (or (alist-get 'p2 spec) (user-error "line needs p2"))
               layer)))
     ((string= type "text")
      (anvil-cad--entity-from-pairs
       "TEXT" (anvil-cad--text-pairs
               (or (alist-get 'text spec) "")
               (or (alist-get 'p1 spec) (alist-get 'at spec)
                   (user-error "text needs p1/at"))
               layer
               (or (alist-get 'height spec) 2.5))))
     ((string= type "circle")
      (anvil-cad--entity-from-pairs
       "CIRCLE" (anvil-cad--circle-pairs
                 (or (alist-get 'center spec) (alist-get 'at spec)
                     (user-error "circle needs center/at"))
                 (or (alist-get 'radius spec) (user-error "circle needs radius"))
                 layer)))
     (t (user-error "anvil-cad: unsupported generate type %S" type)))))

;;;; --- editing operations (pure) ------------------------------------------

(defun anvil-cad--replace-in-pairs-text (pairs find replace)
  "Replace literal FIND with REPLACE in code 1/3 values of PAIRS.
Returns (CHANGED-P . NEW-PAIRS)."
  (let ((changed nil))
    (cons (progn
            (setq pairs
                  (mapcar
                   (lambda (pr)
                     (if (and (memq (car pr) '(1 3))
                              (string-match-p (regexp-quote find) (cdr pr)))
                         (progn (setq changed t)
                                (cons (car pr)
                                      (string-replace find replace (cdr pr))))
                       pr))
                   pairs))
            changed)
          pairs)))

(defun anvil-cad--set-layer-in-pairs (pairs layer)
  "Set the code-8 (layer) value of PAIRS to LAYER, adding one if absent.
Returns (CHANGED-P . NEW-PAIRS)."
  (let ((found nil))
    (let ((out (mapcar (lambda (pr)
                         (if (= (car pr) 8)
                             (progn (setq found t) (cons 8 layer))
                           pr))
                       pairs)))
      (cons t (if found out (cons (cons 8 layer) out))))))

(defun anvil-cad--rebuild-entity (e pairs)
  "Return a fresh IR entity for E's type built from PAIRS."
  (anvil-cad--entity-from-pairs (upcase (symbol-name (plist-get e :type))) pairs))

;;;; --- host-side write helpers --------------------------------------------

(defun anvil-cad--require-input (abs)
  "Return the format (dxf/svg) of existing file ABS, or signal `user-error'."
  (unless (file-exists-p abs)
    (user-error "anvil-cad: %s does not exist" abs))
  (let ((fmt (anvil-cad--detect-format abs)))
    (unless (memq fmt '(dxf svg))
      (user-error "anvil-cad: editing supports .dxf/.svg only; got %s" abs))
    fmt))

(defun anvil-cad--write (path content)
  "Write CONTENT to PATH as UTF-8/LF; return the number of bytes written."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'no-message))
  (length (encode-coding-string content 'utf-8)))

(defun anvil-cad--nonempty (s)
  "Return trimmed S when it is a non-empty string, else nil."
  (and s (stringp s)
       (let ((tr (string-trim s))) (and (not (string-empty-p tr)) tr))))

;;;; --- MCP tool handlers (Phase 2: write) ---------------------------------

(defun anvil-cad--tool-annotate (path text x y &optional layer height out-path)
  "Add a TEXT annotation to a DXF or SVG drawing.

MCP Parameters:
  path     - Absolute path to an existing .dxf or .svg file (string).
  text     - Annotation string (string); UTF-8 / Japanese supported.
  x        - Insertion point X coordinate (string, number).
  y        - Insertion point Y coordinate (string, number).
  layer    - Optional target layer (string).  DXF: layer (default \"0\").
             SVG: written as the element's class attribute.
  height   - Optional text height/font-size (string, number).  Default 2.5.
  out-path - Optional output path (string).  When empty the input file
             is edited in place; all other content is preserved.

Returns a printed plist (:ok t :op annotate :format FMT :out PATH ...)."
  (anvil-server-with-error-handling
   (let* ((abs (expand-file-name path))
          (fmt (anvil-cad--require-input abs))
          (src (anvil-cad--slurp abs))
          (lay (anvil-cad--nonempty layer))
          (h (if (anvil-cad--nonempty height) (string-to-number height) 2.5))
          (xn (string-to-number x))
          (yn (string-to-number y))
          (out (or (and (anvil-cad--nonempty out-path) (expand-file-name out-path))
                   abs))
          (rendered
           (if (eq fmt 'svg)
               (anvil-cad--xml-serialize
                (anvil-cad--svg-append
                 (anvil-cad--xml-parse src)
                 (list (anvil-cad--svg-make-text text xn yn lay h))))
             (anvil-cad--render-dxf
              src (append (plist-get (anvil-cad--dxf-parse src) :entities)
                          (list (anvil-cad--entity-from-pairs
                                 "TEXT" (anvil-cad--text-pairs
                                         text (list xn yn) (or lay "0") h)))))))
          (bytes (anvil-cad--write out rendered)))
     (format "%S" (list :ok t :op 'annotate :format fmt :out out :added 1
                        :text text :layer lay :at (list xn yn) :bytes bytes)))))

(defun anvil-cad--tool-batch-update (path &optional layer type match-text
                                          find replace set-layer out-path)
  "Batch-edit entity text and/or layer across a DXF or SVG drawing.

Selects entities by the optional LAYER, TYPE and MATCH-TEXT filters,
then applies the requested change(s) to each: literal FIND->REPLACE in
its text, and/or moving it to SET-LAYER.  For SVG the edits apply to
<text> elements and `layer' maps to the class attribute (TYPE filter
is ignored — only text is editable).

MCP Parameters:
  path       - Absolute path to an existing .dxf or .svg file (string).
  layer      - Optional layer filter (string): DXF layer / SVG class.
  type       - Optional type filter (string, e.g. \"text\"; DXF only).
  match-text - Optional content filter (string): only entities whose
               text contains it.
  find       - Literal substring to replace in matched entities' text.
  replace    - Replacement string (may be empty to delete FIND).
  set-layer  - Optional new layer (DXF) / class (SVG) for matches.
  out-path   - Optional output path (string).  Empty = edit in place.

At least one of FIND (with REPLACE) or SET-LAYER must be given.
Returns (:ok t :op batch-update :format FMT :changed N ...)."
  (anvil-server-with-error-handling
   (let* ((abs (expand-file-name path))
          (fmt (anvil-cad--require-input abs))
          (src (anvil-cad--slurp abs))
          (lay-f (anvil-cad--nonempty layer))
          (type-f (and (anvil-cad--nonempty type)
                       (intern (downcase (anvil-cad--nonempty type)))))
          (match (anvil-cad--nonempty match-text))
          (find* (anvil-cad--nonempty find))
          (set-lay (anvil-cad--nonempty set-layer))
          (out (or (and (anvil-cad--nonempty out-path) (expand-file-name out-path))
                   abs)))
     (unless (or find* set-lay)
       (user-error "anvil-cad: batch-update needs find (+replace) or set-layer"))
     (if (eq fmt 'svg)
         ;; SVG: edit <text> elements on the DOM (layer = class attribute).
         (let* ((dom (anvil-cad--xml-parse src))
                (cnt (list 0))
                (root (plist-get dom :root))
                (new-root (and root (anvil-cad--svg-tree-edit
                                     root find* replace set-lay lay-f match cnt)))
                (new-dom (list :prologue (plist-get dom :prologue) :root new-root))
                (bytes (anvil-cad--write out (anvil-cad--xml-serialize new-dom))))
           (format "%S" (list :ok t :op 'batch-update :format 'svg :out out
                              :changed (car cnt) :bytes bytes)))
       ;; DXF: rewrite entity pairs.
       (let* ((entities (plist-get (anvil-cad--dxf-parse src) :entities))
              (changed 0)
              (new-entities
               (mapcar
                (lambda (e)
                  (if (and (or (null lay-f) (equal (plist-get e :layer) lay-f))
                           (or (null type-f) (eq (plist-get e :type) type-f))
                           (or (null match)
                               (and (plist-get e :text)
                                    (string-match-p (regexp-quote match)
                                                    (plist-get e :text)))))
                      (let ((pairs (plist-get e :pairs)) (ch nil))
                        (when find*
                          (let ((r (anvil-cad--replace-in-pairs-text
                                    pairs find* (or replace ""))))
                            (when (car r) (setq ch t) (setq pairs (cdr r)))))
                        (when set-lay
                          (let ((r (anvil-cad--set-layer-in-pairs pairs set-lay)))
                            (setq ch t) (setq pairs (cdr r))))
                        (if ch
                            (progn (setq changed (1+ changed))
                                   (anvil-cad--rebuild-entity e pairs))
                          e))
                    e))
                entities))
              (bytes (anvil-cad--write
                      out (anvil-cad--render-dxf src new-entities))))
         (format "%S" (list :ok t :op 'batch-update :format 'dxf :out out
                            :changed changed :total (length entities)
                            :bytes bytes)))))))

(defun anvil-cad--tool-generate (out-path entities-json &optional base-path overwrite)
  "Generate entities programmatically into a new or existing drawing.
The output format (DXF or SVG) is chosen from OUT-PATH's extension.

MCP Parameters:
  out-path     - Absolute path of the .dxf or .svg file to write (string).
  entities-json- JSON array of entity specs (string).  Each object:
                 {\"type\":\"line\",\"layer\":\"L\",\"p1\":[0,0],\"p2\":[10,0]}
                 {\"type\":\"text\",\"layer\":\"L\",\"text\":\"R1\",\"p1\":[1,1],\"height\":2.5}
                 {\"type\":\"circle\",\"layer\":\"L\",\"center\":[5,5],\"radius\":3}
  base-path    - Optional existing drawing (string) of the SAME format as
                 OUT-PATH.  When given, the new entities are appended to it
                 and all other content is preserved; otherwise a minimal
                 skeleton is produced (R12 DXF / standalone SVG).
  overwrite    - Optional flag (non-empty string).  Required to clobber
                 an existing OUT-PATH (unless OUT-PATH equals BASE-PATH).

Returns (:ok t :op generate :format FMT :out PATH :generated N ...)."
  (anvil-server-with-error-handling
   (let* ((out (expand-file-name out-path))
          (fmt (anvil-cad--detect-format out))
          (_ (unless (memq fmt '(dxf svg))
               (user-error "anvil-cad: out-path must be .dxf/.svg; got %s" out)))
          (specs (json-parse-string entities-json
                                    :object-type 'alist :array-type 'list))
          (overwrite-p (and (anvil-cad--nonempty overwrite) t))
          (base (anvil-cad--nonempty base-path))
          (babs (and base (expand-file-name base))))
     (when base
       (unless (file-exists-p babs)
         (user-error "anvil-cad: base %s does not exist" babs))
       (unless (eq (anvil-cad--detect-format babs) fmt)
         (user-error "anvil-cad: base format must match out-path (%s)" fmt)))
     (let* ((n 0)
            (rendered
             (if (eq fmt 'svg)
                 (let ((nodes (mapcar #'anvil-cad--svg-spec-to-node specs)))
                   (setq n (length nodes))
                   (if base
                       (anvil-cad--xml-serialize
                        (anvil-cad--svg-append (anvil-cad--xml-parse
                                                (anvil-cad--slurp babs))
                                               nodes))
                     (anvil-cad--xml-serialize (anvil-cad--svg-skeleton nodes))))
               (let ((ents (mapcar #'anvil-cad--spec-to-entity specs)))
                 (setq n (length ents))
                 (if base
                     (anvil-cad--render-dxf
                      (anvil-cad--slurp babs)
                      (append (plist-get (anvil-cad--dxf-parse
                                          (anvil-cad--slurp babs))
                                         :entities)
                              ents))
                   (anvil-cad--skeleton-dxf ents))))))
       (when (and (file-exists-p out) (not overwrite-p)
                  (not (and babs (string= out babs))))
         (user-error "anvil-cad: %s exists; pass overwrite to replace" out))
       (let ((bytes (anvil-cad--write out rendered)))
         (format "%S" (list :ok t :op 'generate :format fmt :out out
                            :generated n :base base :bytes bytes)))))))

;;;; --- SVG: minimal XML parser/serializer (pure, dual-target) -------------
;;
;; NeLisp has no libxml, so SVG support rests on a small recursive-descent
;; XML parser written in portable Elisp.  It covers the SVG subset we need:
;; the XML/DOCTYPE/comment prologue (kept verbatim), nested elements with
;; quoted attributes, self-closing tags, text, comments, CDATA, character
;; entities, and raw-text style/script bodies.  DOM nodes:
;;   (el TAG ATTRS CHILDREN)  ATTRS = ((name . value)..), order preserved
;;   (txt STRING)             decoded text
;;   (rawtxt STRING)          verbatim (CDATA / style / script)
;;   (comment STRING)
;; A document is (:prologue STR :root NODE).  Re-serializing is
;; data-lossless (every element/attribute/text survives) though not
;; byte-identical (formatting/quoting may normalize) — SVG renders the same.

(defun anvil-cad--str-replace (from to s)
  "Literal replace FROM with TO in S (portable; no string-replace needed)."
  (replace-regexp-in-string (regexp-quote from) to s t t))

(defun anvil-cad--strpos (needle str from)
  "Char index of literal NEEDLE in STR at/after FROM, or nil.
Match data is preserved so callers can interleave freely."
  (save-match-data (string-match (regexp-quote needle) str from)))

(defun anvil-cad--xml-looking-at (prefix str i)
  "Return non-nil when STR has literal PREFIX starting at index I."
  (let ((pl (length prefix)))
    (and (<= (+ i pl) (length str))
         (string= (substring str i (+ i pl)) prefix))))

(defun anvil-cad--xml-decode (s)
  "Decode XML character entities in S (named + numeric)."
  (if (not (anvil-cad--strpos "&" s 0)) s
    (let ((n (length s)) (i 0) (out nil))
      (while (< i n)
        (let ((c (aref s i)))
          (if (eq c ?&)
              (let ((semi (anvil-cad--strpos ";" s i)))
                (if (and semi (< (- semi i) 12))
                    (let ((ent (substring s (1+ i) semi)))
                      (setq out
                            (cons
                             (cond
                              ((string= ent "lt") "<")
                              ((string= ent "gt") ">")
                              ((string= ent "amp") "&")
                              ((string= ent "quot") "\"")
                              ((string= ent "apos") "'")
                              ((and (> (length ent) 1) (eq (aref ent 0) ?#)
                                    (eq (aref ent 1) ?x))
                               (char-to-string (string-to-number (substring ent 2) 16)))
                              ((and (> (length ent) 0) (eq (aref ent 0) ?#))
                               (char-to-string (string-to-number (substring ent 1))))
                              (t (concat "&" ent ";")))
                             out))
                      (setq i (1+ semi)))
                  (progn (setq out (cons "&" out)) (setq i (1+ i)))))
            (progn (setq out (cons (char-to-string c) out)) (setq i (1+ i))))))
      (apply #'concat (nreverse out)))))

(defun anvil-cad--xml-encode-text (s)
  "Encode &, <, > for XML text content."
  (anvil-cad--str-replace ">" "&gt;"
   (anvil-cad--str-replace "<" "&lt;"
    (anvil-cad--str-replace "&" "&amp;" s))))

(defun anvil-cad--xml-encode-attr (s)
  "Encode &, <, \" for an XML attribute value."
  (anvil-cad--str-replace "\"" "&quot;"
   (anvil-cad--str-replace "<" "&lt;"
    (anvil-cad--str-replace "&" "&amp;" s))))

(defun anvil-cad--xml-name-char-p (c)
  "Return non-nil when C may appear in an XML name."
  (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
      (and (>= c ?0) (<= c ?9)) (memq c '(?: ?- ?_ ?.))))

(defun anvil-cad--xml-skip-ws (str i)
  "Return the index of the first non-whitespace char in STR at/after I."
  (let ((n (length str)))
    (while (and (< i n) (memq (aref str i) '(?\s ?\t ?\n ?\r))) (setq i (1+ i)))
    i))

(defun anvil-cad--xml-parse-name (str i)
  "Parse an XML name from STR at I.  Return (NAME . NEXT-INDEX)."
  (let ((n (length str)) (start i))
    (while (and (< i n) (anvil-cad--xml-name-char-p (aref str i))) (setq i (1+ i)))
    (cons (substring str start i) i)))

(defun anvil-cad--xml-parse-attrs (str i)
  "Parse attributes from STR at I until `>' or `/'.  Return (ATTRS . INDEX)."
  (let ((n (length str)) (attrs nil) (done nil))
    (while (not done)
      (setq i (anvil-cad--xml-skip-ws str i))
      (if (or (>= i n) (memq (aref str i) '(?> ?/)))
          (setq done t)
        (let* ((np (anvil-cad--xml-parse-name str i)) (name (car np)))
          (setq i (cdr np))
          (if (string-empty-p name)
              (setq i (1+ i))         ; stray char: skip to avoid a stall
            (progn
              (setq i (anvil-cad--xml-skip-ws str i))
              (if (and (< i n) (eq (aref str i) ?=))
                  (let ((j (anvil-cad--xml-skip-ws str (1+ i))))
                    (let* ((q (aref str j))
                           (end (or (anvil-cad--strpos (char-to-string q) str (1+ j)) n)))
                      (setq attrs (cons (cons name (anvil-cad--xml-decode
                                                    (substring str (1+ j) end)))
                                        attrs))
                      (setq i (1+ end))))
                (setq attrs (cons (cons name "") attrs))))))))
    (cons (nreverse attrs) i)))

(defun anvil-cad--xml-parse-element (str i)
  "Parse an element from STR at I (str[I] = `<').  Return (NODE . INDEX)."
  (let ((n (length str)))
    (setq i (1+ i))
    (let* ((np (anvil-cad--xml-parse-name str i)) (tag (car np)))
      (setq i (cdr np))
      (let* ((ap (anvil-cad--xml-parse-attrs str i)) (attrs (car ap)))
        (setq i (cdr ap))
        (setq i (anvil-cad--xml-skip-ws str i))
        (cond
         ((and (< i n) (eq (aref str i) ?/))
          (setq i (1+ i))
          (when (and (< i n) (eq (aref str i) ?>)) (setq i (1+ i)))
          (cons (list 'el tag attrs nil) i))
         ((and (< i n) (eq (aref str i) ?>))
          (setq i (1+ i))
          (if (member (downcase tag) '("style" "script"))
              (let* ((close (concat "</" tag))
                     (cpos (or (anvil-cad--strpos close str i) n))
                     (raw (substring str i cpos))
                     (gt (or (anvil-cad--strpos ">" str cpos) (1- n))))
                (cons (list 'el tag attrs (list (list 'rawtxt raw))) (1+ gt)))
            (let ((children nil) (loop t))
              (while loop
                (cond
                 ((>= i n) (setq loop nil))
                 ((and (eq (aref str i) ?<) (< (1+ i) n) (eq (aref str (1+ i)) ?/))
                  (let ((gt (or (anvil-cad--strpos ">" str i) (1- n))))
                    (setq i (1+ gt) loop nil)))
                 ((anvil-cad--xml-looking-at "<!--" str i)
                  (let ((e (or (anvil-cad--strpos "-->" str i) n)))
                    (setq children (cons (list 'comment (substring str (+ i 4) e)) children))
                    (setq i (min n (+ e 3)))))
                 ((anvil-cad--xml-looking-at "<![CDATA[" str i)
                  (let ((e (or (anvil-cad--strpos "]]>" str i) n)))
                    (setq children (cons (list 'rawtxt (substring str (+ i 9) e)) children))
                    (setq i (min n (+ e 3)))))
                 ((and (eq (aref str i) ?<) (< (1+ i) n)
                       (anvil-cad--xml-name-char-p (aref str (1+ i))))
                  (let ((cp (anvil-cad--xml-parse-element str i)))
                    (setq children (cons (car cp) children))
                    (setq i (cdr cp))))
                 ((eq (aref str i) ?<)
                  (let ((gt (or (anvil-cad--strpos ">" str i) (1- n)))) (setq i (1+ gt))))
                 (t
                  (let ((lt (or (anvil-cad--strpos "<" str i) n)))
                    (setq children (cons (list 'txt (anvil-cad--xml-decode
                                                     (substring str i lt)))
                                         children))
                    (setq i lt)))))
              (cons (list 'el tag attrs (nreverse children)) i))))
         (t (cons (list 'el tag attrs nil) i)))))))

(defun anvil-cad--xml-root-start (str)
  "Return the index where the root element begins, skipping the prologue."
  (let ((i 0) (n (length str)) (res nil))
    (while (and (not res) (< i n))
      (let ((c (aref str i)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\r)) (setq i (1+ i)))
         ((and (eq c ?<) (< (1+ i) n) (eq (aref str (1+ i)) ??))
          (let ((e (anvil-cad--strpos "?>" str i))) (setq i (if e (+ e 2) n))))
         ((anvil-cad--xml-looking-at "<!--" str i)
          (let ((e (anvil-cad--strpos "-->" str i))) (setq i (if e (+ e 3) n))))
         ((and (eq c ?<) (< (1+ i) n) (eq (aref str (1+ i)) ?!))
          (let ((e (anvil-cad--strpos ">" str i))) (setq i (if e (1+ e) n))))
         ((eq c ?<) (setq res i))
         (t (setq i (1+ i))))))
    (or res n)))

(defun anvil-cad--xml-parse (str)
  "Parse XML STR into (:prologue STR :root NODE)."
  (let* ((rs (anvil-cad--xml-root-start str))
         (prologue (substring str 0 rs)))
    (if (>= rs (length str))
        (list :prologue prologue :root nil)
      (list :prologue prologue :root (car (anvil-cad--xml-parse-element str rs))))))

(defun anvil-cad--xml-serialize-node (node)
  "Serialize a DOM NODE back to XML text."
  (cond
   ((eq (car node) 'txt) (anvil-cad--xml-encode-text (nth 1 node)))
   ((eq (car node) 'rawtxt) (nth 1 node))
   ((eq (car node) 'comment) (concat "<!--" (nth 1 node) "-->"))
   ((eq (car node) 'el)
    (let* ((tag (nth 1 node)) (attrs (nth 2 node)) (children (nth 3 node))
           (attr-str (mapconcat
                      (lambda (a)
                        (concat " " (car a) "=\""
                                (anvil-cad--xml-encode-attr (cdr a)) "\""))
                      attrs "")))
      (if (null children)
          (concat "<" tag attr-str "/>")
        (concat "<" tag attr-str ">"
                (mapconcat #'anvil-cad--xml-serialize-node children "")
                "</" tag ">"))))
   (t "")))

(defun anvil-cad--xml-serialize (dom)
  "Serialize a parsed DOM document (:prologue/:root) to XML text."
  (concat (plist-get dom :prologue)
          (let ((root (plist-get dom :root)))
            (if root (anvil-cad--xml-serialize-node root) ""))))

;;;; --- SVG: DOM <-> IR mapping (pure) -------------------------------------

(defun anvil-cad--svg-num (s)
  "Parse leading number from SVG length string S (units ignored).
Always returns a float for uniformity with the DXF adapter.  Default 0.0."
  (if (and s (stringp s)) (float (string-to-number s)) 0.0))

(defun anvil-cad--svg-pt (xs ys)
  "Return point (X Y 0.0) from string coords XS/YS, or nil when both absent."
  (and (or xs ys) (list (anvil-cad--svg-num xs) (anvil-cad--svg-num ys) 0.0)))

(defun anvil-cad--svg-node-text (node)
  "Concatenate all descendant text of NODE (tspan etc. flattened)."
  (cond
   ((eq (car node) 'txt) (nth 1 node))
   ((eq (car node) 'rawtxt) (nth 1 node))
   ((eq (car node) 'el) (mapconcat #'anvil-cad--svg-node-text (nth 3 node) ""))
   (t "")))

(defun anvil-cad--svg-parse-points (s)
  "Parse an SVG points string S into a list of (X Y 0.0)."
  (if (or (null s) (string-empty-p (string-trim (or s ""))))
      nil
    (let* ((toks (split-string (string-trim s) "[ ,\n\t\r]+" t))
           (nums (mapcar (lambda (tok) (float (string-to-number tok))) toks))
           (out nil))
      (while (and nums (cdr nums))
        (setq out (cons (list (car nums) (cadr nums) 0.0) out))
        (setq nums (cddr nums)))
      (nreverse out))))

(defun anvil-cad--ir (tag layer attrs text p1 p2 vtx)
  "Assemble an IR entity plist (used by the SVG adapter)."
  (append (list :type (intern (downcase tag)))
          (and layer (list :layer layer))
          (and text (list :text text))
          (and p1 (list :p1 p1))
          (and p2 (list :p2 p2))
          (and vtx (> vtx 1) (list :vertices vtx))
          (list :attrs attrs)))

(defun anvil-cad--svg-elt-to-ir (tag attrs node layer)
  "Map an SVG element (TAG/ATTRS/NODE) on LAYER to an IR entity.
The effective layer is the inherited LAYER (an Inkscape `<g>' layer) or,
absent that, the element's own `data-layer' attribute."
  (let ((layer (or layer (cdr (assoc "data-layer" attrs))))
        (g (lambda (k) (cdr (assoc k attrs)))))
    (cond
     ((string= tag "line")
      (anvil-cad--ir tag layer attrs nil
                     (anvil-cad--svg-pt (funcall g "x1") (funcall g "y1"))
                     (anvil-cad--svg-pt (funcall g "x2") (funcall g "y2")) nil))
     ((string= tag "circle")
      (anvil-cad--ir tag layer attrs nil
                     (anvil-cad--svg-pt (funcall g "cx") (funcall g "cy")) nil nil))
     ((string= tag "ellipse")
      (anvil-cad--ir tag layer attrs nil
                     (anvil-cad--svg-pt (funcall g "cx") (funcall g "cy")) nil nil))
     ((string= tag "rect")
      (anvil-cad--ir tag layer attrs nil
                     (anvil-cad--svg-pt (funcall g "x") (funcall g "y")) nil nil))
     ((string= tag "text")
      (anvil-cad--ir tag layer attrs
                     (string-trim (anvil-cad--svg-node-text node))
                     (anvil-cad--svg-pt (funcall g "x") (funcall g "y")) nil nil))
     ((member tag '("polyline" "polygon"))
      (let ((pts (anvil-cad--svg-parse-points (funcall g "points"))))
        (anvil-cad--ir tag layer attrs nil (car pts) nil (length pts))))
     ((string= tag "path")
      (anvil-cad--ir tag layer attrs nil nil nil nil)))))

(defun anvil-cad--svg-walk (node layer)
  "Collect IR entities from DOM NODE, threading the current LAYER."
  (if (not (eq (car node) 'el)) nil
    (let ((tag (nth 1 node)) (attrs (nth 2 node)) (children (nth 3 node)) (out nil))
      (cond
       ((string= tag "g")
        (let* ((lbl (or (cdr (assoc "inkscape:label" attrs))
                        (and (equal (cdr (assoc "inkscape:groupmode" attrs)) "layer")
                             (cdr (assoc "id" attrs)))))
               (lay (or lbl layer)))
          (dolist (ch children)
            (setq out (append out (anvil-cad--svg-walk ch lay))))))
       ((member tag '("line" "circle" "ellipse" "rect" "text" "polyline"
                      "polygon" "path"))
        (setq out (list (anvil-cad--svg-elt-to-ir tag attrs node layer))))
       (t (dolist (ch children)
            (setq out (append out (anvil-cad--svg-walk ch layer))))))
      out)))

(defun anvil-cad--svg-parse (text)
  "Parse SVG TEXT into the same plist shape as `anvil-cad--dxf-parse'.
Adds :dom (the parsed document) for the write path."
  (let* ((dom (anvil-cad--xml-parse text))
         (root (plist-get dom :root))
         (entities (and root (anvil-cad--svg-walk root nil))))
    (list :format 'svg :sections nil
          :entities entities
          :layers (anvil-cad--distinct-layers entities)
          :dom dom)))

;;;; --- SVG: build / edit DOM (pure) ---------------------------------------

(defun anvil-cad--alist-put (key val alist)
  "Return ALIST with KEY set to VAL (appended if absent), order preserved."
  (let ((found nil))
    (let ((out (mapcar (lambda (a)
                         (if (equal (car a) key)
                             (progn (setq found t) (cons key val))
                           a))
                       alist)))
      (if found out (append alist (list (cons key val)))))))

(defun anvil-cad--svg-make-text (text x y layer height)
  "Build an SVG <text> DOM node for TEXT at X,Y, with optional LAYER/HEIGHT."
  (list 'el "text"
        (append (list (cons "x" (anvil-cad--fmt-num x))
                      (cons "y" (anvil-cad--fmt-num y)))
                (and height (list (cons "font-size" (anvil-cad--fmt-num height))))
                (and layer (list (cons "data-layer" layer))))
        (list (list 'txt text))))

(defun anvil-cad--svg-spec-to-node (spec)
  "Build an SVG DOM node from a generate SPEC alist (symbol keys)."
  (let* ((type (downcase (or (alist-get 'type spec)
                             (user-error "anvil-cad: spec missing `type'"))))
         (layer (alist-get 'layer spec))
         (cls (and layer (list (cons "data-layer" layer)))))
    (cond
     ((string= type "line")
      (let ((p1 (or (alist-get 'p1 spec) (user-error "line needs p1")))
            (p2 (or (alist-get 'p2 spec) (user-error "line needs p2"))))
        (list 'el "line"
              (append (list (cons "x1" (anvil-cad--fmt-num (nth 0 p1)))
                            (cons "y1" (anvil-cad--fmt-num (nth 1 p1)))
                            (cons "x2" (anvil-cad--fmt-num (nth 0 p2)))
                            (cons "y2" (anvil-cad--fmt-num (nth 1 p2)))
                            (cons "stroke" "black"))
                      cls)
              nil)))
     ((string= type "text")
      (let ((p (or (alist-get 'p1 spec) (alist-get 'at spec)
                   (user-error "text needs p1/at"))))
        (anvil-cad--svg-make-text (or (alist-get 'text spec) "")
                                  (nth 0 p) (nth 1 p) layer
                                  (or (alist-get 'height spec) 2.5))))
     ((string= type "circle")
      (let ((c (or (alist-get 'center spec) (alist-get 'at spec)
                   (user-error "circle needs center/at")))
            (r (or (alist-get 'radius spec) (user-error "circle needs radius"))))
        (list 'el "circle"
              (append (list (cons "cx" (anvil-cad--fmt-num (nth 0 c)))
                            (cons "cy" (anvil-cad--fmt-num (nth 1 c)))
                            (cons "r" (anvil-cad--fmt-num r))
                            (cons "fill" "none") (cons "stroke" "black"))
                      cls)
              nil)))
     (t (user-error "anvil-cad: unsupported generate type %S" type)))))

(defun anvil-cad--svg-append (dom nodes)
  "Return DOM with NODES appended to the root element's children."
  (let ((root (plist-get dom :root)))
    (unless (and root (eq (car root) 'el))
      (user-error "anvil-cad: SVG has no root element to append to"))
    (list :prologue (plist-get dom :prologue)
          :root (list 'el (nth 1 root) (nth 2 root)
                      (append (nth 3 root) nodes)))))

(defun anvil-cad--svg-skeleton (nodes)
  "Return a standalone SVG document whose root contains NODES."
  (list :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        :root (list 'el "svg"
                    (list (cons "xmlns" "http://www.w3.org/2000/svg")
                          (cons "version" "1.1"))
                    nodes)))

(defun anvil-cad--svg-tree-edit (node find replace set-layer layer-f match cnt)
  "Edit <text> elements of DOM NODE: literal FIND->REPLACE and/or SET-LAYER.
LAYER-F filters by class; MATCH by current content.  CNT is a one-element
list used as a mutable changed-counter.  Returns the rebuilt node."
  (if (not (eq (car node) 'el)) node
    (let ((tag (nth 1 node)) (attrs (nth 2 node)) (children (nth 3 node)))
      (if (string= tag "text")
          (let* ((content (anvil-cad--svg-node-text node))
                 (lay-ok (or (null layer-f)
                             (equal (cdr (assoc "data-layer" attrs)) layer-f)))
                 (match-ok (or (null match)
                               (string-match-p (regexp-quote match) content)))
                 (find-ok (or (null find)
                              (string-match-p (regexp-quote find) content))))
            (if (and lay-ok match-ok find-ok)
                (let ((new-attrs attrs) (new-children children) (changed nil))
                  (when find
                    (setq new-children
                          (list (list 'txt (anvil-cad--str-replace
                                            find (or replace "") content))))
                    (setq changed t))
                  (when set-layer
                    (setq new-attrs (anvil-cad--alist-put "data-layer" set-layer attrs))
                    (setq changed t))
                  (when changed (setcar cnt (1+ (car cnt))))
                  (list 'el tag new-attrs new-children))
              node))
        (list 'el tag attrs
              (mapcar (lambda (ch)
                        (anvil-cad--svg-tree-edit ch find replace set-layer
                                                  layer-f match cnt))
                      children))))))

;;;; --- module lifecycle ---------------------------------------------------

;;;###autoload
(defun anvil-cad-enable ()
  "Register the anvil-cad MCP tools."
  (anvil-server-register-tool
   #'anvil-cad--tool-read-outline
   :id "cad-read-outline"
   :intent '(file-read)
   :layer 'core
   :server-id anvil-cad--server-id
   :read-only t
   :description
   "Summarize a CAD drawing's structure: sections, layers, entity-type
census and bounding box.  Supports ASCII DXF and SVG.  Use this FIRST to
orient before pulling entities with `cad-extract'.")
  (anvil-server-register-tool
   #'anvil-cad--tool-extract
   :id "cad-extract"
   :intent '(file-read)
   :layer 'core
   :server-id anvil-cad--server-id
   :read-only t
   :description
   "Extract CAD drawing entities as structured IR (type/layer/text/points),
with optional layer and type filters.  Supports ASCII DXF and SVG.  Ideal
for pulling annotations, labels and geometry out of electrical drawings.")
  (anvil-server-register-tool
   #'anvil-cad--tool-annotate
   :id "cad-annotate"
   :intent '(file-write)
   :layer 'core
   :server-id anvil-cad--server-id
   :description
   "Add a TEXT annotation (label/note) to a DXF or SVG drawing at a given
point, layer and height.  Edits in place unless an out-path is supplied;
all other content is preserved.")
  (anvil-server-register-tool
   #'anvil-cad--tool-batch-update
   :id "cad-batch-update"
   :intent '(file-write)
   :layer 'core
   :server-id anvil-cad--server-id
   :description
   "Batch-edit entity text (literal find/replace) and/or layer across a
DXF or SVG drawing, scoped by optional layer/type/match-text filters.
Ideal for title-block dates, management numbers and bulk relabelling.")
  (anvil-server-register-tool
   #'anvil-cad--tool-generate
   :id "cad-generate"
   :intent '(file-write)
   :layer 'core
   :server-id anvil-cad--server-id
   :description
   "Generate entities (line/text/circle) from a JSON spec into a new
DXF or SVG drawing (chosen by out-path extension) or appended to an
existing one.  Programmatic drafting for single-line diagram elements,
labels and symbols.")
  t)

(defun anvil-cad-disable ()
  "Unregister the anvil-cad MCP tools."
  (anvil-server-unregister-tool "cad-read-outline" anvil-cad--server-id)
  (anvil-server-unregister-tool "cad-extract" anvil-cad--server-id)
  (anvil-server-unregister-tool "cad-annotate" anvil-cad--server-id)
  (anvil-server-unregister-tool "cad-batch-update" anvil-cad--server-id)
  (anvil-server-unregister-tool "cad-generate" anvil-cad--server-id)
  t)

(provide 'anvil-cad)
;;; anvil-cad.el ends here
