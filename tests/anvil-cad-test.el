;;; anvil-cad-test.el --- Tests for anvil-cad -*- lexical-binding: t; -*-

(require 'ert)
(require 'anvil-cad)

;;;; --- fixtures -----------------------------------------------------------

(defconst anvil-cad-test--dxf
  (concat
   "0\nSECTION\n2\nHEADER\n9\n$ACADVER\n1\nAC1027\n0\nENDSEC\n"
   "0\nSECTION\n2\nENTITIES\n"
   ;; a LINE on layer ELEC
   "0\nLINE\n8\nELEC\n10\n0.0\n20\n0.0\n30\n0.0\n11\n100.0\n21\n50.0\n31\n0.0\n"
   ;; a TEXT (Japanese content) on layer LABEL
   "0\nTEXT\n8\nLABEL\n10\n10.0\n20\n20.0\n40\n2.5\n1\nR1 受電\n"
   ;; a CIRCLE on layer ELEC
   "0\nCIRCLE\n8\nELEC\n10\n50.0\n20\n50.0\n40\n5.0\n"
   "0\nENDSEC\n0\nEOF\n")
  "Minimal valid ASCII DXF with HEADER + ENTITIES (line, text, circle).")

(defconst anvil-cad-test--dxf-crlf
  (replace-regexp-in-string "\n" "\r\n" anvil-cad-test--dxf)
  "Same fixture with CRLF line endings (AutoCAD's default).")

;;;; --- scaffold smoke -----------------------------------------------------

(ert-deftest anvil-cad-test-feature-provided ()
  "The module's feature symbol is provided after load."
  (should (featurep 'anvil-cad)))

(ert-deftest anvil-cad-test-enable-disable-callable ()
  "Enable and disable stubs exist and return without error."
  (should (fboundp 'anvil-cad-enable))
  (should (fboundp 'anvil-cad-disable)))

;;;; --- pair / parse core --------------------------------------------------

(ert-deftest anvil-cad-test-pairs-basic ()
  "Group code/value pairs are read in order with integer codes."
  (let ((pairs (anvil-cad--dxf-pairs "0\nSECTION\n2\nENTITIES\n0\nEOF\n")))
    (should (equal pairs '((0 . "SECTION") (2 . "ENTITIES") (0 . "EOF"))))))

(ert-deftest anvil-cad-test-parse-sections ()
  "Section names are collected in order."
  (let ((p (anvil-cad--dxf-parse anvil-cad-test--dxf)))
    (should (equal (plist-get p :sections) '("HEADER" "ENTITIES")))
    (should (eq (plist-get p :format) 'dxf))))

(ert-deftest anvil-cad-test-parse-entity-count-and-types ()
  "All three ENTITIES-section entities are parsed with correct types."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (es (plist-get p :entities)))
    (should (= (length es) 3))
    (should (equal (mapcar (lambda (e) (plist-get e :type)) es)
                   '(line text circle)))))

(ert-deftest anvil-cad-test-parse-line ()
  "LINE entity yields layer and both endpoints."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (line (nth 0 (plist-get p :entities))))
    (should (equal (plist-get line :layer) "ELEC"))
    (should (equal (plist-get line :p1) '(0.0 0.0 0.0)))
    (should (equal (plist-get line :p2) '(100.0 50.0 0.0)))))

(ert-deftest anvil-cad-test-parse-text ()
  "TEXT entity yields its (Japanese) content, layer and insertion point."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (txt (nth 1 (plist-get p :entities))))
    (should (equal (plist-get txt :layer) "LABEL"))
    (should (equal (plist-get txt :text) "R1 受電"))
    (should (equal (plist-get txt :p1) '(10.0 20.0 0.0)))))

(ert-deftest anvil-cad-test-parse-crlf ()
  "CRLF line endings parse identically to LF."
  (should (equal (anvil-cad--dxf-parse anvil-cad-test--dxf-crlf)
                 (anvil-cad--dxf-parse anvil-cad-test--dxf))))

(ert-deftest anvil-cad-test-layers ()
  "Distinct layers are collected in first-seen order."
  (let ((p (anvil-cad--dxf-parse anvil-cad-test--dxf)))
    (should (equal (plist-get p :layers) '("ELEC" "LABEL")))))

;;;; --- outline ------------------------------------------------------------

(ert-deftest anvil-cad-test-outline ()
  "Outline reports count, type census and bounding box."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (o (anvil-cad--outline p)))
    (should (= (plist-get o :entity-count) 3))
    (should (equal (sort (copy-sequence (plist-get o :entity-types))
                         (lambda (a b) (string< (symbol-name (car a))
                                                (symbol-name (car b)))))
                   '((circle . 1) (line . 1) (text . 1))))
    ;; bbox over points (0,0)(100,50)(10,20)(50,50)
    (should (equal (plist-get o :bbox) '(0.0 0.0 100.0 50.0)))))

;;;; --- filtering ----------------------------------------------------------

(ert-deftest anvil-cad-test-filter-by-layer ()
  "Layer filter keeps only matching entities."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (es (plist-get p :entities))
         (elec (anvil-cad--filter-entities es "ELEC" nil)))
    (should (= (length elec) 2))
    (should (cl-every (lambda (e) (equal (plist-get e :layer) "ELEC")) elec))))

(ert-deftest anvil-cad-test-filter-by-type ()
  "Type filter keeps only matching entities."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (es (plist-get p :entities))
         (txt (anvil-cad--filter-entities es nil 'text)))
    (should (= (length txt) 1))
    (should (eq (plist-get (car txt) :type) 'text))))

(ert-deftest anvil-cad-test-strip-pairs ()
  "strip-pairs removes :pairs but keeps convenience fields."
  (let* ((p (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (line (nth 0 (plist-get p :entities)))
         (stripped (anvil-cad--strip-pairs line)))
    (should (null (plist-get stripped :pairs)))
    (should (equal (plist-get stripped :layer) "ELEC"))
    (should (equal (plist-get stripped :p1) '(0.0 0.0 0.0)))))

;;;; --- Phase 2: write core (pure) -----------------------------------------

(defun anvil-cad-test--plist (res)
  "Read a handler's printed-plist RES string back into a plist."
  (car (read-from-string res)))

(ert-deftest anvil-cad-test-fmt-num ()
  "Numbers serialize with a decimal point; strings pass through."
  (should (equal (anvil-cad--fmt-num 3) "3.0"))
  (should (equal (anvil-cad--fmt-num 2.5) "2.5"))
  (should (equal (anvil-cad--fmt-num "1.0") "1.0")))

(ert-deftest anvil-cad-test-pairs-to-text ()
  "Pairs serialize to alternating code/value lines; nil -> empty."
  (should (equal (anvil-cad--pairs-to-text '((0 . "LINE") (8 . "L")))
                 "0\nLINE\n8\nL\n"))
  (should (equal (anvil-cad--pairs-to-text nil) "")))

(ert-deftest anvil-cad-test-split-region ()
  "PRE ends at 2/ENTITIES and POST starts at the closing 0/ENDSEC."
  (let ((r (anvil-cad--split-entities-region anvil-cad-test--dxf)))
    (should r)
    (should (equal (car (last (nth 0 r))) '(2 . "ENTITIES")))
    (should (equal (car (nth 1 r)) '(0 . "ENDSEC")))))

(ert-deftest anvil-cad-test-roundtrip-lossless ()
  "parse -> render (same entities) -> parse preserves entities/sections.
This is the guarantee that makes in-place edits safe."
  (let* ((p1 (anvil-cad--dxf-parse anvil-cad-test--dxf))
         (rendered (anvil-cad--render-dxf anvil-cad-test--dxf
                                          (plist-get p1 :entities)))
         (p2 (anvil-cad--dxf-parse rendered)))
    (should (equal (plist-get p2 :sections) (plist-get p1 :sections)))
    (should (equal (plist-get p2 :layers) (plist-get p1 :layers)))
    (should (equal (plist-get p2 :entities) (plist-get p1 :entities)))))

(ert-deftest anvil-cad-test-render-no-entities-section ()
  "A drawing without an ENTITIES section gets one synthesized before EOF."
  (let* ((base "0\nSECTION\n2\nHEADER\n9\n$ACADVER\n1\nAC1009\n0\nENDSEC\n0\nEOF\n")
         (ent (anvil-cad--entity-from-pairs
               "TEXT" (anvil-cad--text-pairs "X" '(0 0) "0" 1)))
         (parsed (anvil-cad--dxf-parse (anvil-cad--render-dxf base (list ent)))))
    (should (= (length (plist-get parsed :entities)) 1))
    (should (member "ENTITIES" (plist-get parsed :sections)))
    (should (member "HEADER" (plist-get parsed :sections)))))

;;;; --- Phase 2: handlers (end-to-end, temp files) -------------------------

(ert-deftest anvil-cad-test-annotate ()
  "cad-annotate appends a TEXT entity and preserves the rest in place."
  (let ((f (make-temp-file "anvil-cad-" nil ".dxf" anvil-cad-test--dxf)))
    (unwind-protect
        (let* ((res (anvil-cad-test--plist
                     (anvil-cad--tool-annotate f "新規注記" "5" "7" "NOTES" "3.0")))
               (es (plist-get (anvil-cad--dxf-parse (anvil-cad--slurp f)) :entities))
               (added (car (last es))))
          (should (plist-get res :ok))
          (should (= (length es) 4))
          (should (eq (plist-get added :type) 'text))
          (should (equal (plist-get added :text) "新規注記"))
          (should (equal (plist-get added :layer) "NOTES"))
          (should (equal (plist-get added :p1) '(5.0 7.0 0.0))))
      (delete-file f))))

(ert-deftest anvil-cad-test-batch-update-text ()
  "cad-batch-update does literal find/replace on text entities."
  (let ((f (make-temp-file "anvil-cad-" nil ".dxf" anvil-cad-test--dxf)))
    (unwind-protect
        (let* ((res (anvil-cad-test--plist
                     (anvil-cad--tool-batch-update f nil "text" nil
                                                   "受電" "高圧受電")))
               (txt (nth 1 (plist-get (anvil-cad--dxf-parse (anvil-cad--slurp f))
                                      :entities))))
          (should (= (plist-get res :changed) 1))
          (should (equal (plist-get txt :text) "R1 高圧受電")))
      (delete-file f))))

(ert-deftest anvil-cad-test-batch-update-set-layer ()
  "cad-batch-update moves filtered entities to a new layer."
  (let ((f (make-temp-file "anvil-cad-" nil ".dxf" anvil-cad-test--dxf)))
    (unwind-protect
        (let* ((res (anvil-cad-test--plist
                     (anvil-cad--tool-batch-update f "ELEC" nil nil nil nil
                                                   "POWER")))
               (parsed (anvil-cad--dxf-parse (anvil-cad--slurp f))))
          (should (= (plist-get res :changed) 2))
          (should (member "POWER" (plist-get parsed :layers)))
          (should-not (member "ELEC" (plist-get parsed :layers))))
      (delete-file f))))

(ert-deftest anvil-cad-test-generate-standalone ()
  "cad-generate builds a standalone DXF from a JSON spec."
  (let ((f (make-temp-file "anvil-cad-gen-" nil ".dxf")))
    (unwind-protect
        (let* ((json (concat "[{\"type\":\"line\",\"layer\":\"L\","
                             "\"p1\":[0,0],\"p2\":[10,0]},"
                             "{\"type\":\"text\",\"layer\":\"L\",\"text\":\"A\","
                             "\"p1\":[1,1]},"
                             "{\"type\":\"circle\",\"layer\":\"L\","
                             "\"center\":[5,5],\"radius\":3}]"))
               (res (anvil-cad-test--plist
                     (anvil-cad--tool-generate f json nil "1")))
               (es (plist-get (anvil-cad--dxf-parse (anvil-cad--slurp f)) :entities)))
          (should (= (plist-get res :generated) 3))
          (should (= (length es) 3))
          (should (equal (mapcar (lambda (e) (plist-get e :type)) es)
                         '(line text circle)))
          (should (equal (plist-get (nth 0 es) :p2) '(10.0 0.0 0.0)))
          (should (equal (plist-get (nth 1 es) :text) "A"))
          (should (equal (plist-get (nth 2 es) :p1) '(5.0 5.0 0.0))))
      (delete-file f))))

(ert-deftest anvil-cad-test-generate-append ()
  "cad-generate appends to a base drawing, preserving its sections."
  (let ((base (make-temp-file "anvil-cad-base-" nil ".dxf" anvil-cad-test--dxf))
        (out (make-temp-file "anvil-cad-out-" nil ".dxf")))
    (unwind-protect
        (let* ((json (concat "[{\"type\":\"text\",\"layer\":\"NEW\","
                             "\"text\":\"追加\",\"p1\":[9,9]}]"))
               (res (anvil-cad-test--plist
                     (anvil-cad--tool-generate out json base "1")))
               (parsed (anvil-cad--dxf-parse (anvil-cad--slurp out)))
               (es (plist-get parsed :entities)))
          (should (= (plist-get res :generated) 1))
          (should (= (length es) 4))
          (should (equal (plist-get (car (last es)) :text) "追加"))
          (should (equal (plist-get parsed :sections) '("HEADER" "ENTITIES"))))
      (delete-file base)
      (delete-file out))))

(ert-deftest anvil-cad-test-generate-refuses-clobber ()
  "cad-generate refuses to overwrite an existing file without the flag.
The handler signals (via `anvil-server-tool-throw') rather than writing."
  (let ((f (make-temp-file "anvil-cad-gen-" nil ".dxf" "existing")))
    (unwind-protect
        (should-error
         (anvil-cad--tool-generate
          f "[{\"type\":\"text\",\"text\":\"x\",\"p1\":[0,0]}]"))
      (delete-file f))))

;;;; --- Phase 3: SVG ------------------------------------------------------

(defconst anvil-cad-test--svg
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\""
   " width=\"200\" height=\"100\">\n"
   "  <!-- electrical -->\n"
   "  <style>.lbl{font-size:4px}</style>\n"
   "  <g inkscape:label=\"配線\" inkscape:groupmode=\"layer\" id=\"layer1\">\n"
   "    <line x1=\"0\" y1=\"0\" x2=\"100\" y2=\"50\" stroke=\"black\"/>\n"
   "    <text x=\"10\" y=\"20\" class=\"lbl\">R1 &amp; 受電</text>\n"
   "  </g>\n"
   "  <circle cx=\"50\" cy=\"50\" r=\"5\" fill=\"none\"/>\n"
   "</svg>\n")
  "Representative SVG: prologue, comment, style, an Inkscape layer group
with a line + entity-bearing text, and a top-level circle.")

(ert-deftest anvil-cad-test-xml-entities ()
  "Entity decode/encode round-trips through the XML helpers."
  (should (equal (anvil-cad--xml-decode "a &lt;b&gt; &amp; &#65;") "a <b> & A"))
  (should (equal (anvil-cad--xml-encode-text "a <b> & c") "a &lt;b&gt; &amp; c"))
  (should (equal (anvil-cad--xml-encode-attr "x\"&<") "x&quot;&amp;&lt;")))

(ert-deftest anvil-cad-test-svg-parse-ir ()
  "SVG maps to IR with types, inherited layer, decoded text and points."
  (let* ((p (anvil-cad--svg-parse anvil-cad-test--svg))
         (es (plist-get p :entities)))
    (should (eq (plist-get p :format) 'svg))
    (should (equal (mapcar (lambda (e) (plist-get e :type)) es)
                   '(line text circle)))
    (should (equal (plist-get (nth 0 es) :layer) "配線"))
    (should (equal (plist-get (nth 0 es) :p1) '(0.0 0.0 0.0)))
    (should (equal (plist-get (nth 0 es) :p2) '(100.0 50.0 0.0)))
    (should (equal (plist-get (nth 1 es) :layer) "配線"))
    (should (equal (plist-get (nth 1 es) :text) "R1 & 受電"))
    (should (equal (plist-get (nth 1 es) :p1) '(10.0 20.0 0.0)))
    (should (null (plist-get (nth 2 es) :layer)))
    (should (equal (plist-get p :layers) '("配線")))))

(ert-deftest anvil-cad-test-svg-xml-roundtrip ()
  "parse -> serialize -> parse is data-lossless for entities and layers."
  (let* ((p1 (anvil-cad--svg-parse anvil-cad-test--svg))
         (p2 (anvil-cad--svg-parse
              (anvil-cad--xml-serialize (plist-get p1 :dom)))))
    (should (equal (plist-get p2 :entities) (plist-get p1 :entities)))
    (should (equal (plist-get p2 :layers) (plist-get p1 :layers)))))

(ert-deftest anvil-cad-test-svg-style-preserved ()
  "The raw <style> body survives a parse/serialize round-trip."
  (let ((out (anvil-cad--xml-serialize
              (anvil-cad--xml-parse anvil-cad-test--svg))))
    (should (string-match-p "<style>\\.lbl{font-size:4px}</style>" out))
    (should (string-match-p "<!-- electrical -->" out))))

(ert-deftest anvil-cad-test-svg-outline ()
  "Outline works on SVG: count, census and bbox."
  (let ((o (anvil-cad--outline (anvil-cad--svg-parse anvil-cad-test--svg))))
    (should (= (plist-get o :entity-count) 3))
    (should (equal (plist-get o :bbox) '(0.0 0.0 100.0 50.0)))))

(ert-deftest anvil-cad-test-svg-annotate ()
  "cad-annotate adds a <text> to an SVG; data-layer becomes its layer."
  (let ((f (make-temp-file "anvil-cad-" nil ".svg" anvil-cad-test--svg)))
    (unwind-protect
        (let* ((res (anvil-cad-test--plist
                     (anvil-cad--tool-annotate f "注記" "5" "7" "NOTES" "3")))
               (es (plist-get (anvil-cad--svg-parse (anvil-cad--slurp f))
                              :entities))
               (added (car (last es))))
          (should (eq (plist-get res :format) 'svg))
          (should (= (length es) 4))
          (should (eq (plist-get added :type) 'text))
          (should (equal (plist-get added :text) "注記"))
          (should (equal (plist-get added :layer) "NOTES"))
          (should (equal (plist-get added :p1) '(5.0 7.0 0.0))))
      (delete-file f))))

(ert-deftest anvil-cad-test-svg-batch-update-text ()
  "cad-batch-update does literal find/replace on SVG <text> content."
  (let ((f (make-temp-file "anvil-cad-" nil ".svg" anvil-cad-test--svg)))
    (unwind-protect
        (let* ((res (anvil-cad-test--plist
                     (anvil-cad--tool-batch-update f nil nil nil
                                                   "受電" "高圧受電")))
               (txt (nth 1 (plist-get (anvil-cad--svg-parse (anvil-cad--slurp f))
                                      :entities))))
          (should (= (plist-get res :changed) 1))
          (should (equal (plist-get txt :text) "R1 & 高圧受電")))
      (delete-file f))))

(ert-deftest anvil-cad-test-svg-generate-standalone ()
  "cad-generate builds a standalone SVG from a JSON spec."
  (let ((f (make-temp-file "anvil-cad-gen-" nil ".svg")))
    (unwind-protect
        (let* ((json (concat "[{\"type\":\"line\",\"layer\":\"L\","
                             "\"p1\":[0,0],\"p2\":[10,0]},"
                             "{\"type\":\"text\",\"layer\":\"L\",\"text\":\"A\","
                             "\"p1\":[1,1]},"
                             "{\"type\":\"circle\",\"layer\":\"L\","
                             "\"center\":[5,5],\"radius\":3}]"))
               (res (anvil-cad-test--plist
                     (anvil-cad--tool-generate f json nil "1")))
               (es (plist-get (anvil-cad--svg-parse (anvil-cad--slurp f))
                              :entities)))
          (should (eq (plist-get res :format) 'svg))
          (should (= (plist-get res :generated) 3))
          (should (equal (mapcar (lambda (e) (plist-get e :type)) es)
                         '(line text circle)))
          (should (equal (plist-get (nth 1 es) :text) "A"))
          (should (equal (plist-get (nth 2 es) :layer) "L")))
      (delete-file f))))

(ert-deftest anvil-cad-test-svg-generate-append ()
  "cad-generate appends to a base SVG and preserves existing entities."
  (let ((base (make-temp-file "anvil-cad-base-" nil ".svg" anvil-cad-test--svg))
        (out (make-temp-file "anvil-cad-out-" nil ".svg")))
    (unwind-protect
        (let* ((json "[{\"type\":\"circle\",\"layer\":\"DEV\",\"center\":[1,1],\"radius\":2}]")
               (res (anvil-cad-test--plist
                     (anvil-cad--tool-generate out json base "1")))
               (es (plist-get (anvil-cad--svg-parse (anvil-cad--slurp out))
                              :entities)))
          (should (= (plist-get res :generated) 1))
          (should (= (length es) 4))
          (should (eq (plist-get (car (last es)) :type) 'circle)))
      (delete-file base)
      (delete-file out))))

(ert-deftest anvil-cad-test-svg-generate-format-mismatch ()
  "cad-generate refuses a base whose format differs from out-path."
  (let ((base (make-temp-file "anvil-cad-base-" nil ".dxf" anvil-cad-test--dxf))
        (out (make-temp-file "anvil-cad-out-" nil ".svg")))
    (unwind-protect
        (should-error
         (anvil-cad--tool-generate
          out "[{\"type\":\"text\",\"text\":\"x\",\"p1\":[0,0]}]" base "1"))
      (delete-file base)
      (delete-file out))))

(provide 'anvil-cad-test)
;;; anvil-cad-test.el ends here
