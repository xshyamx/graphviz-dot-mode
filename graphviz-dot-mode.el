;;; Customizable variables
(defgroup graphviz nil
  "Major mode for graphviz"
  :group 'languages)

(defcustom graphviz-dot-cmd "dot"
  "Path to dot executable"
  :type 'string
  :group 'graphviz)

(defcustom graphviz-force-save-before-preview t
  "Save file before generating preview"
  :type 'boolean
  :group 'graphviz)

(defconst graphviz-ms-paint "mspaint"
  "Constant for MS-Paint executable name")

(defcustom graphviz-preview-program "msphotos"
  "Program to use for opening preview files"
  :type 'string
  :options '(mspaint msphotos)
  :group 'graphviz)

;;; default mode map
(defvar graphviz-dot-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'graphviz-preview)
    (define-key keymap (kbd "C-c C-o") #'graphviz-open-preview)
    (define-key keymap (kbd "C-c !") #'graphviz-bootstrap-template)
    keymap))

(defvar graphviz-dot-mode-hook nil
  "Standard mode hook for graphviz-dot-mode")

;;; syntax table
(defvar graphviz-dot-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124 b" syntax-table) ; 1-1st char of comment-start, 2-2nd char of comment-start, 4-2nd char of comment-close
    (modify-syntax-entry ?* ". 23" syntax-table) ; comment closer for style b
    (modify-syntax-entry ?\n "> b" syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?- "_" syntax-table)
    (modify-syntax-entry ?_ "_" syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    (modify-syntax-entry ?\[ "(]" syntax-table)
    (modify-syntax-entry ?\] ")[" syntax-table)
    (modify-syntax-entry ?\" "\"" syntax-table)
    syntax-table)
  "Syntax table for graphviz-dot-mode")

(defun graphviz--add-prefixes (base prefixes)
  "Return a list of strings with the list of `prefixes' applied to the `base'"
  (mapcar (lambda (x) (concat x base)) prefixes))

(defun graphviz--add-suffixes (base suffixes)
  "Return a list of strings with the list of `suffixes' applied to the `base'"
  (mapcar (lambda (x) (concat base x)) suffixes))

;; constants
(defconst graphviz-component-types
  '("node" "edge" "graph")
  "Default top-level components")

(defconst graphviz--attr-keywords
  `(
    "Damping" "Epsilon" "URL" "bb" "center" "clusterrank" "colorscheme"
    "comment" "compound" "concentrate" "constraint" "decorate" "dim"
    "dir" "distortion" "fixedsize" "group" "height" "layer"
    "layers" "len" "head" "lp" "ltail" "margin" "maxiter" "mclimit"
    "mindist" "minlen" "model" "nodesep" "normalize" "nslimit"
    "nslimitl" "ordering" "orientation" "overlap" "pack" "page"
    "pagedir" "patch" "penwidth" "peripheries" "pin" "pos" "quantum"
    "ratio" "rects" "regular" "remincross" "root" "rotate" "samehead"
    "sametail" "samplepoint" "scale" "searchsize" "sep" "shapefile"
    "showboxes" "sides" "size" "skew" "splines" "start" "strict"
    "stylesheet" "vertices" "voro_margin" "weight" "width" "z"
    ,@(graphviz--add-suffixes "arrow" '("head" "size" "tail"))
    ,@(graphviz--add-suffixes "tail" '("URL" "port" ))
    ,@(graphviz--add-suffixes "head" '("URL" "port" ))
    ,@(graphviz--add-suffixes "font" '("color" "name" "path" "size"))
    ,@(graphviz--add-suffixes "label" '("angle" "distance" "float" "just" "loc" "fontcolor" "fontname" "fontsize"))
    ,@(graphviz--add-suffixes "color" '("" "bg" "fill" "pen" "just" "loc" "fontcolor" "fontname" "fontsize"))
    ,@(graphviz--add-suffixes "rank" '("" "dir" "sep" ))
    ,@(graphviz--add-prefixes "label" '("" "head" "tail" "top" "bottom"))
    )
  "Attributes & keywords"
  )

(defconst graphviz--value-types-arrow
  `(
    ,@(graphviz--add-prefixes "box" '("" "L" "r" "o" "ol" "or"))
    ,@(graphviz--add-prefixes "row" '("c" "lc" "rc"))
    ,@(graphviz--add-prefixes "diamond" '("" "l" "r" "o" "ol" "or"))
    ,@(graphviz--add-prefixes "dot" '("" "o"))
    ,@(graphviz--add-prefixes "inv" '("" "l" "r" "o" "ol" "or"))
    ,@(graphviz--add-prefixes "normal" '("" "l" "r" "o" "ol" "or"))
    ,@(graphviz--add-prefixes "tee" '("" "l" "r"))
    ,@(graphviz--add-prefixes "vee" '("" "l" "r") )
    ,@(graphviz--add-prefixes "curve" '("" "l" "r" "o" "ol" "or"))
    )
  "Possible values for the `arrowType' attribute")

(defconst graphviz--shape-types
  `(
    "assembly" "box" "box3d" "cds" "circle" "component" "cylinder"
    "diamond" "egg" "ellipse" "folder" "house" "insulator" "none"
    "note" "oval" "parallelogram" "plain" "plaintext" "point" "rect"
    "rectangle" "signature" "square" "star" "terminator" "trapezium"
    "triangle" "underline" "utr" "record"
    ,@(graphviz--add-suffixes "M" '("circle" "diamond" "square"))
    ,@(graphviz--add-suffixes "double" '("circle" "octagon" ))
    ,@(graphviz--add-suffixes "inv" '("house" "trapezium" "triangle"))
    ,@(graphviz--add-prefixes "site" '("ribo" "primer" "protease" "restriction"))
    ,@(graphviz--add-prefixes "tab" '("" "rnas" "proteins"))
    ,@(graphviz--add-prefixes "arrow" '("" "l" "r"))
    ,@(graphviz--add-prefixes "promoter" '("" "l" "r"))
    ,@(graphviz--add-prefixes "overhang" '("fivep" "n" "threep"))
    ,@(graphviz--add-prefixes "gon" '("poly" "penta" "hexa" "septa" "octa" "tripleocta"))
    )
  "Possible values for the `shape' attribute")

(defconst graphviz--style-types
  '("dashed" "dotted" "solid" "invis" "bold" "tapered" "filled"
    "striped" "wedged" "diagonals" "rounded" "radial")
  "Possible values for the `style' attribute")

(defconst graphviz--attr-values
  `(
    "back" "both" "c" "false" "forward" "global" "halfopen" "inv" "local"
    "max" "min" "n" "none" "none" "normal" "s" "same" "sink" "tee" "true"
    "LR"
    ,@(graphviz--add-prefixes "box" '("" "o"))
    ,@(graphviz--add-prefixes "diamond" '("" "o" ))
    ,@(graphviz--add-prefixes "empty" '("" "inv"))
    ,@(graphviz--add-suffixes "dot" '("o" "inv" "invo"))
    ,@(graphviz--add-suffixes "e" '("" "n" "s"))
    ,@(graphviz--add-suffixes "W" '("" "n" "s"))
    ,@(graphviz--add-suffixes ":" '("n" "s" "e" "w" "ne" "nw" "se" "sw"))
    ,@(graphviz--add-suffixes "B" '("L" "R"))
    ,@(graphviz--add-suffixes "T" '("L" "R"))
    ,@(graphviz--add-prefixes "L" '("B" "R" "T"))
    ,@(graphviz--add-suffixes "R" '("B" "T"))
    )
  "Possible attribute values")

;; font-lock variables
(defconst graphviz-font-lock-graph
  (list (rx
	 (? "strict" (+ space))
	 (group (or "graph" "digraph")) (+ space)
	 (group (any alpha "_") (* (any alnum "_"))))
	'(1 font-lock-keyword-face)
	'(2 font-lock-function-name-face))
  "font-lock rule for preamble")

(defconst graphviz-font-lock-subgraph
  (list (rx
	 (group "subgraph")
	 (+ space)
	 (group "cluster_" (+ (any alnum " "))))
	'(1 font-lock-keyword-face)
	'(2 font-lock-function-name-face))
  "font-lock rule for subgraphs")

(defconst graphviz-font-lock-attribute-names
  (cons
   (rx-to-string `(seq bow (or ,@graphviz--attr-keywords) eow))
   'font-lock-keyword-face
   )
  "font-lock rule for attributes")

(defconst graphviz-font-lock-attribute-values
  (list
   (rx-to-string `(seq "=" (* space) (group (or (seq (* digit) (? "." (+ digit))) ,@graphviz--attr-values)) eow))
   '(1 font-lock-constant-face)
   )
  "font-lock rule for attribute values")


(defconst graphviz-font-lock-components
  (cons (rx-to-string `(seq bow (or ,@graphviz-component-types) eow) t)
	'font-lock-type-face))

(defconst graphviz-font-lock-arrows
  '("- [>-]" •font-lock-warning-face))

(defconst graphviz-font-lock-shape
  (list
   (rx-to-string `(seq bow (group "shape") (* space) "=" (* space) (group (or ,@graphviz--shape-types))))
   '(1 font-lock-keyword-face)
   '(2 font-lock-preprocessor-face)
   ))

(defconst graphviz-font-lock-style
  (list
   (rx-to-string `(seq bow (group "style") (* space) "=" (* space) (group (or ,@graphviz--style-types))))
   '(1 font-lock-keyword-face)
   '(2 font-lock-preprocessor-face)
   ))

(defconst graphviz-font-lock-port-refs
  (list
   (rx bow (any alpha " ") (* (any alnum " "))
       ":" (group (+ alnum)) eow)
   '(1 font-lock-variable-name-face)
   ))

(defconst graphviz-font-lock-keywords
  `(
    ,graphviz-font-lock-components
    ,graphviz-font-lock-arrows
    ,graphviz-font-lock-graph
    ,graphviz-font-lock-subgraph
    ,graphviz-font-lock-shape
    ,graphviz-font-lock-style
    ,graphviz-font-lock-attribute-names
    ,graphviz-font-lock-attribute-values
    ,graphviz-font-lock-port-refs
    )
  "Default font-lock-keywords")

;; autoloads
;;;###autoload
(define-derived-mode graphviz-dot-mode prog-mode "dot"
  "Major mode for GraphViz dot language"
  (setq-local font-lock-defaults '(graphviz-font-lock-keywords))
  (add-hook 'xref-backend-functions #'graphviz-xref-backend)
  (run-hooks 'graphviz-dot-mode-hook)
  )

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\.\\(gv\\/dot\\)\\'" - graphviz-dot-mode) t)

;;; preview functions
(defun graphviz--preview-process (filename)
  "Launch process to generate preview and return the running process"
  (let ((default-directory (file-name-directory filename))
	(basename (file-name-nondirectory filename))
	(output-file (concat (file-name-sans-extension (file-name-nondirectory filename)) ".png")))
    (start-process
     "*graphviz*" "*graphviz*" graphviz-dot-cmd
     "-Tpng" "-o" output-file basename))
  )

(defun graphviz--preview-process-sentinel (proc event)
  "Sentinel function to print status of preview process"
  (if (equal "finished\n" event)
      (message "Generated preview successfully")
    (user-error "Generating preview failed: %s" event)))

(defun graphviz-preview ()
  "Spawn a process to generate png preview file. Use with universal
argument (C-u) to remove any existing preview png file."
  (interactive)
					; save file
  (when graphviz-force-save-before-preview
    (save-buffer))
					; delete old file
  (when current-prefix-arg
    (let ((preview-file (concat (file-name-sans-extension (buffer-file-name)) ".png" )))
      (when (file-exists-p preview-file)
	(delete-file preview-file))))
  (graphviz--run-preview (buffer-file-name) #'graphviz--preview-process-sentinel)
  )

(defun graphviz--run-preview (filename sentinel)
  "Runs the preview for the file with the sentinel on completion"
  (if (file-exists-p filename)
      (let ((ps (graphviz--preview-process filename)))
	(process-put ps :filename (concat (file-name-sans-extension filename) ".one"))
	(set-process-sentinel ps sentinel))
    (user-error "File not present. Save file before previewing" )))

(defun graphviz--open-preview-sentinel (proc event)
  "Sentinel function to open preview file"
  (if (equal "finished\n" event)
      (graphviz--open-preview-program (process-get proc :filename))
    (user-error "Generating preview failed")))

(defun graphviz--open-preview-program (filename)
  "Launch MS-Paint to preview generated png file"
  (let ((default-directory (file-name-directory filename))
	(target-file (file-name-nondirectory filename))
	(buffer-name "*graphviz-open-preview*" ))
    (if (string-equal graphviz-preview-program graphviz-ms-paint)
	(start-process buffer-name nil graphviz-preview-program target-file)
      (if (fboundp 'w32-shell-execute)
	  (w32-shell-execute "open" target-file)
	(shell-command (concat "open " target-file))))
    ))
(defun graphviz-open-preview ()
  "Open generated preview png file in MS-Paint"
  (interactive)
  ;; save if necessary
  (when graphviz-force-save-before-preview
    (save-buffer))
  ;; delete previous preview file
  (when current-prefix-arg
    (let ((preview-file (concat (file-name-sans-extension (buffer-file-name)) ".png")))
      (when (file-exists-p preview-file)
	(delete-file preview-file))))
  (graphviz--run-preview (buffer-file-name) #'graphviz--open-preview-sentinel)
  )

(defun graphviz-bootstrap-template ()
  "Starter bootstrap for graphviz diagram"
  (interactive)
  (goto-char 0)
  (let ((start "diagrap G {\n  "))
    (insert (format "%s\n}" start))
    (goto-char (1+ (length start)))))

(defmacro graphviz-full-buffer-search (buffer &rest body)
  `(with-current-buffer ,buffer
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	,@body))))

(defun graphviz--find-defs ()
  (graphviz-full-buffer-search
   (current-buffer)
   (let (matches)
     (while (re-search-forward
	     (rx bow (group alpha (* (any alnum "_"))) eow "[") nil t)
       (unless (member (match-string-no-properties 1) graphviz-component-types)
	 (push (match-string-no-properties 1) matches)))
     (reverse matches))))

(defun graphviz--find-definitions (symbol)
  (message "graphviz--find-definitions(%s)" symbol)
  (graphviz-full-buffer-search
   (current-buffer)
   (let (matches)
     (while (re-search-forward
	     (rx-to-string `(seq bow (group ,symbol) eow (* whitespace) "["))
	     nil t)
       (push (xref-make
	      (match-string 1)
	      (xref-make-buffer-location (current-buffer) (match-beginning 1)))
	     matches))
     (reverse matches))))

(defun graphviz--find-references (symbol)
  (graphviz-full-buffer-search
   (current-buffer)
   (let ((regexp (rx-to-string `(seq bow ,symbol eow))) (matches))
     (while (re-search-forward regexp nil t)
       (push (xref-make
	      (format "%s: %s"
		      (propertize (format "%d" (line-number-at-pos)) 'font-lock-face 'shadow)
		      (buffer-substring (line-beginning-position) (line-end-position)))
	      (xref-make-buffer-location (current-buffer) (match-beginning 0)))
	     matches))
     (reverse matches))))


(defun graphviz-xref-backend ()
  "xref backend for grapvhiz files"
  'graphviz)

(cl-defmethod xref-backend-identifier-at-point ((_ (eql graphviz)))
  "Return the identifier to lookup"
  (symbol-name (symbol-at-point)))

;; xref-backend
(cl-defmethod xref-backend-identifier-completion-table ((_ (eql graphviz)))
  "Return list of terms for completion from the current buffer"
  (graphviz--find-defs))

(cl-defmethod xref-backend-definitions ((_ (eql graphviz)) symbol)
  ;; (message "graphviz-xref (%s) : %s" (buffer-name) symbol)
  (graphviz--find-definitions symbol))

(cl-defmethod xref-backend-references ((_ (eql graphviz)) symbol)
  "List of references matching symbol"
  (graphviz--find-references symbol))

(provide 'graphviz-dot-mode)
