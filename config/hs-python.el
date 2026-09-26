;; hs-python.el — hs-minor-mode for Python: blocks, cells, brackets, strings, imports, comments  -*- lexical-binding: t -*-
;;
;; Replaces the Python hideshow hooks (python.el's, which even in Emacs 31 fold
;; only def/class/if-style blocks and miss indented comment blocks) with ones
;; that also fold:
;;
;;   • Cells — VS Code / Jupyter `# %% label' headers.  A cell runs from its
;;     header to the line before the next header at the same or lower
;;     indentation, or until the code dedents below the header (so a
;;     `    # %% fitting' inside a class ends with the class).  Folding a
;;     header hides the whole cell, header line stays visible.
;;   • Brackets — a (, [ or { that closes on a later line folds to `x = (⋯)',
;;     keeping the closer and anything after it visible.
;;   • Triple-quoted strings (docstrings included) spanning lines fold to
;;     `"""First line⋯"""'.
;;   • Import groups — consecutive import statements at one indentation
;;     (blank lines and comments between isort sections included) fold to the
;;     first import.
;;   • Comment blocks — runs of consecutive whole-line comments at any
;;     indentation; blank lines and cell headers split them.  Inline comments
;;     (`x = 1  # ...') are never blocks.
;;
;; Level-based commands (`hs-hide-all', `hs-hide-level', and the nested steps
;; of `hs-cycle') treat cells as transparent: they fold what is inside cells
;; and leave the headers visible as section dividers.  Only commands aimed at a
;; header itself (`hs-toggle-hiding', `hs-hide-block', `hs-cycle' on the header
;; line) fold a cell.
;;
;; Fringe indicators (`hs-show-indicators') mark every foldable block,
;; comment blocks included.
;;
;; Needs the Emacs 31 hideshow API (vendored in lisp/hideshow-31 on Emacs 30).

;; %% line predicates

(defconst my/hs-py-cell-re "^[ \t]*#[ \t]*%%"
  "Regexp matching a `# %%' cell header at the beginning of a line.")

(defconst my/hs-py--import-re "[ \t]*\\(?:import\\|from\\)\\_>"
  "Regexp matching an import statement from the beginning of a line.")

(defconst my/hs-py--opener-re "[([{]\\|\"\"\"\\|'''"
  "Regexp matching a bracket or triple-quote opener.")

(defvar my/hs-py--cells-transparent nil
  "Non-nil while hideshow walks blocks level by level; cells are then not blocks.")

(defun my/hs-py--line-start-p ()
  "Non-nil if the current line begins a logical line.
That is, its start is not inside a string or brackets and the previous line
does not end with a backslash continuation."
  (save-excursion
    (forward-line 0)
    (let ((ppss (syntax-ppss)))
      (and (not (nth 3 ppss))
           (zerop (car ppss))
           (not (and (> (point) (1+ (point-min)))
                     (eq (char-before (1- (point))) ?\\)
                     (not (nth 8 (syntax-ppss (1- (point)))))))))))

(defun my/hs-py--cell-header-p ()
  "Non-nil if the current line is a cell header; match data is set on it."
  (and (my/hs-py--line-start-p)
       (save-excursion
         (forward-line 0)
         (looking-at my/hs-py-cell-re))))

(defun my/hs-py--comment-line-p ()
  "Non-nil if the line at point is a whole-line comment that is not a cell header."
  (and (looking-at-p "^[ \t]*#")
       (not (looking-at-p my/hs-py-cell-re))
       (not (nth 3 (syntax-ppss)))))

(defun my/hs-py--import-line-p ()
  "Non-nil if the current line starts an import statement."
  (and (my/hs-py--line-start-p)
       (save-excursion
         (forward-line 0)
         (looking-at-p my/hs-py--import-re))))

(defun my/hs-py--skip-blank-lines (dir)
  "Move DIR (1 or -1) lines while on blank or whole-line comment lines.
Return nil if the buffer edge was reached first."
  (let ((ok t))
    (while (and (setq ok (zerop (forward-line dir)))
                (not (eobp))
                (looking-at-p "[ \t]*\\(?:#.*\\)?$")))
    (and ok (not (eobp)))))

;; %% extents

(defun my/hs-py--cell-end ()
  "Return the end of the cell whose header is on the current line.
Trailing blank lines, and trailing comments dedented below the header (they
introduce whatever follows the cell), are left outside the cell."
  (save-excursion
    (forward-line 0)
    (let ((indent (current-indentation))
          (last (pos-eol)))
      (forward-line 1)
      (while (and (not (eobp))
                  (not (and (not (looking-at-p "[ \t]*$"))
                            (my/hs-py--line-start-p)
                            (if (looking-at-p my/hs-py-cell-re)
                                (<= (current-indentation) indent)
                              (and (not (looking-at-p "[ \t]*#"))
                                   (< (current-indentation) indent))))))
        (unless (or (looking-at-p "[ \t]*$")
                    (and (looking-at-p "[ \t]*#")
                         (< (current-indentation) indent)))
          (setq last (pos-eol)))
        (forward-line 1))
      last)))

(defun my/hs-py--enclosing-cell (pos)
  "Return the header position of the innermost cell containing POS, or nil."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (let (found)
      (while (and (not found) (re-search-backward my/hs-py-cell-re nil t))
        (when (and (my/hs-py--line-start-p)
                   (<= pos (my/hs-py--cell-end)))
          (setq found (point))))
      found)))

(defun my/hs-py--opener-close (pos)
  "If POS opens a bracket or triple-quoted string closing on a later line,
return the position of its closing delimiter, else nil."
  (save-excursion
    (goto-char pos)
    (unless (nth 8 (syntax-ppss pos))
      (cond
       ((memq (char-after) '(?\( ?\[ ?\{))
        (let ((close (ignore-errors (scan-lists pos 1 0))))
          (and close (> close (pos-eol)) (1- close))))
       ;; python.el puts the string fence on the third opening quote and on
       ;; the first closing one
       ((looking-at-p "\"\"\"\\|'''")
        (syntax-propertize (point-max))
        (let ((inside (syntax-ppss (+ pos 3))))
          (when (eq (nth 8 inside) (+ pos 2))
            (goto-char (+ pos 3))
            (parse-partial-sexp (point) (point-max) nil nil inside 'syntax-table)
            (and (not (nth 3 (syntax-ppss)))
                 (> (point) (save-excursion (goto-char pos) (pos-eol)))
                 (1- (point))))))))))

(defun my/hs-py--import-group-start-p ()
  "Non-nil if the import on the current line is the first of its group."
  (save-excursion
    (forward-line 0)
    (let ((indent (current-indentation)))
      (or (not (my/hs-py--skip-blank-lines -1))
          (progn
            (python-nav-beginning-of-statement)
            (not (and (= (current-indentation) indent)
                      (my/hs-py--import-line-p))))))))

(defun my/hs-py--import-group-end ()
  "Return the end of the import group whose first import is on the current line."
  (save-excursion
    (forward-line 0)
    (let ((indent (current-indentation)) end)
      (while (progn
               (python-nav-end-of-statement)
               (setq end (point))
               (and (my/hs-py--skip-blank-lines 1)
                    (= (current-indentation) indent)
                    (my/hs-py--import-line-p))))
      end)))

(defun my/hs-py--enclosing-import-group (pos)
  "Return the indentation position of the import group containing POS, or nil."
  (save-excursion
    (goto-char pos)
    (python-nav-beginning-of-statement)
    (when (my/hs-py--import-line-p)
      (let ((indent (current-indentation)))
        (while (not (my/hs-py--import-group-start-p))
          (my/hs-py--skip-blank-lines -1)
          (python-nav-beginning-of-statement))
        (when (= (current-indentation) indent)
          (back-to-indentation)
          (point))))))

(defun my/hs-py--enclosing-opener (pos)
  "Return the innermost multi-line bracket or string opener around POS, or nil."
  (let* ((ppss (syntax-ppss pos))
         (str (and (nth 3 ppss)
                   (> (nth 8 ppss) (+ (point-min) 1))
                   (- (nth 8 ppss) 2))))
    (if (and str (my/hs-py--opener-close str))
        str
      (seq-find #'my/hs-py--opener-close (reverse (nth 9 ppss))))))

;; %% hideshow hooks

(defun my/hs-py--block-header-line-p ()
  "Non-nil if the current line starts a code block (def, class, if, ...)."
  (and (my/hs-py--line-start-p)
       (save-excursion
         (forward-line 0)
         (looking-at-p python-nav-beginning-of-block-regexp))))

(defun my/hs-py-adjust-block-beginning (beg)
  "`hs-adjust-block-beginning-function' for Python.
Fold a code block after its whole header, however many lines it spans; fold
brackets, strings, cells, imports and comments after their first line."
  (save-excursion
    (goto-char beg)
    (if (and (not (memq (char-before) '(?\( ?\[ ?\{ ?\" ?\')))
             (my/hs-py--block-header-line-p))
        (progn (python-nav-end-of-statement) (point))
      (pos-eol))))

(defun my/hs-py-find-next-block (_regexp bound comments)
  "`hs-find-next-block-function' for Python.
From point towards BOUND, find the next block start: a line starting a
code block, cell, import group or (if COMMENTS) comment, or else a
multi-line bracket or string opener.  Leave point after it, the match data
on it, and return non-nil."
  (let (found)
    (while (and (not found) (< (point) bound))
      (let ((ind (save-excursion (back-to-indentation) (point))))
        (if (and (<= (point) ind)
                 (< ind bound)
                 (my/hs-py--line-start-p)
                 (save-excursion
                   (forward-line 0)
                   (cond ((looking-at my/hs-py-cell-re) t)
                         ((looking-at "[ \t]*#")
                          ;; a comment block starts once, at its first line
                          (and comments
                               (save-excursion
                                 (not (and (zerop (forward-line -1))
                                           (my/hs-py--comment-line-p))))
                               (looking-at "[ \t]*#")))
                         ((looking-at-p my/hs-py--import-re)
                          (and (my/hs-py--import-group-start-p)
                               (looking-at my/hs-py--import-re)))
                         (t (looking-at python-nav-beginning-of-block-regexp)))))
            (progn (goto-char (match-end 0))
                   (setq found t))
          (let ((lim (min bound (pos-eol))) pos)
            ;; brackets in a block header (a def's parameters) are part of it
            (when (my/hs-py--block-header-line-p)
              (goto-char lim))
            (goto-char (max (point) ind))
            (while (and (not pos) (re-search-forward my/hs-py--opener-re lim t))
              (let ((beg (match-beginning 0)))
                (when (my/hs-py--opener-close beg)
                  (setq pos beg))))
            (if (not pos)
                (forward-line 1)
              (goto-char pos)
              (looking-at my/hs-py--opener-re)
              (goto-char (match-end 0))
              (setq found t))))))
    found))

(defun my/hs-py-looking-at-block-start-p ()
  "`hs-looking-at-block-start-predicate' for Python; set match data on the start."
  (cond ((my/hs-py--cell-header-p)
         (not my/hs-py--cells-transparent))
        ((my/hs-py--opener-close (point))
         (looking-at my/hs-py--opener-re))
        ((and (my/hs-py--import-line-p)
              (<= (point) (save-excursion (back-to-indentation) (point)))
              (my/hs-py--import-group-start-p))
         (save-excursion
           (forward-line 0)
           (looking-at my/hs-py--import-re)))
        (t (python-info-looking-at-beginning-of-block))))

(defun my/hs-py-forward-sexp (arg)
  "`hs-forward-sexp-function' for Python: move to the end of the block at point.
For brackets and strings that is the closing delimiter, so it stays visible."
  (let (close)
    (cond ((my/hs-py--cell-header-p)
           (goto-char (my/hs-py--cell-end)))
          ((setq close (my/hs-py--opener-close (point)))
           (goto-char close))
          ((my/hs-py--import-line-p)
           (goto-char (my/hs-py--import-group-end)))
          (t (python-hideshow-forward-sexp-function arg)))))

(defun my/hs-py-find-block-beginning ()
  "`hs-find-block-beginning-function' for Python.
Move to the start of the innermost block around point: code block, cell,
bracket, string or import group."
  (let* ((here (point))
         ;; python.el may return a preceding block that ends before point
         (code (save-excursion
                 (when (and (python-nav-beginning-of-block)
                            (save-excursion
                              (python-hideshow-forward-sexp-function 1)
                              (>= (point) here)))
                   (point))))
         (starts (delq nil (list code
                                 (my/hs-py--enclosing-cell here)
                                 (my/hs-py--enclosing-opener here)
                                 (my/hs-py--enclosing-import-group here)))))
    (when starts
      (goto-char (apply #'max starts)))))

(defun my/hs-py-inside-comment-p ()
  "`hs-inside-comment-predicate' for Python.
If point is on a whole-line comment, return (BEG END): the end of the first
line of its comment block and the end of the block's last line."
  (save-excursion
    (forward-line 0)
    (when (my/hs-py--comment-line-p)
      (let ((first (point)) (last (point)))
        (save-excursion
          (while (and (zerop (forward-line -1)) (my/hs-py--comment-line-p))
            (setq first (point))))
        (save-excursion
          (while (and (zerop (forward-line 1)) (not (eobp)) (my/hs-py--comment-line-p))
            (setq last (point))))
        (list (save-excursion (goto-char first) (pos-eol))
              (save-excursion (goto-char last) (pos-eol)))))))

(defun my/hs-py--indicate-comments (fn &rest args)
  "Run FN with ARGS so fringe indicators also mark comment blocks.
Around `hs--add-indicators', which otherwise looks for code blocks only."
  (if (not (derived-mode-p 'python-base-mode))
      (apply fn args)
    (let ((first-block (symbol-function 'hs-get-first-block-on-line)))
      (cl-letf (((symbol-function 'hs-get-first-block-on-line)
                 (lambda (&optional _include-comments) (funcall first-block t))))
        (apply fn args)))))

(defun my/hs-py--transparent-cells (fn &rest args)
  "Run FN with ARGS while cells are not blocks (around `hs-hide-level-recursive')."
  (let ((my/hs-py--cells-transparent t))
    (apply fn args)))

;; %% setup

(defun my/hs-py-setup ()
  "Install the Python hideshow hooks when `hs-minor-mode' turns on."
  (when (and hs-minor-mode (derived-mode-p 'python-base-mode))
    (if (not (boundp 'hs-inside-comment-predicate))
        (message "hs-python: needs the Emacs 31 hideshow (lisp/hideshow-31)")
      (setq-local hs-find-next-block-function #'my/hs-py-find-next-block
                  hs-looking-at-block-start-predicate #'my/hs-py-looking-at-block-start-p
                  hs-forward-sexp-function #'my/hs-py-forward-sexp
                  hs-adjust-block-beginning-function #'my/hs-py-adjust-block-beginning
                  hs-find-block-beginning-function #'my/hs-py-find-block-beginning
                  hs-inside-comment-predicate #'my/hs-py-inside-comment-p))))

(add-hook 'hs-minor-mode-hook #'my/hs-py-setup)

(with-eval-after-load 'hideshow
  (advice-add 'hs-hide-level-recursive :around #'my/hs-py--transparent-cells)
  (advice-add 'hs--add-indicators :around #'my/hs-py--indicate-comments))
