;; hs-indentation-folding.el — hs-minor-mode extension for indentation-based modes
;;
;; Extends `hs-minor-mode' to fold blocks in modes that express structure through
;; indentation rather than explicit delimiters (e.g. yaml-ts-mode).
;;
;; Features:
;;   • Registers yaml-ts-mode in `hs-special-modes-alist' with a custom
;;     `forward-sexp' function (`my/hs-forward-sexp-by-indent') that walks forward
;;     by comparing indentation levels rather than matching brackets.
;;   • Folds only real block headers — lines ending with `:` — so leaf key-value
;;     pairs and comment lines are never accidentally collapsed.
;;   • Blank lines between sibling blocks are preserved (excluded from the hidden
;;     region) so the buffer doesn't reflow when blocks are collapsed.
;;   • Consecutive comment lines (`# …`) are folded as a single comment block,
;;     showing only the first line plus the fold indicator.
;;   • Replaces the default plain `...` ellipsis with a styled Unicode mid-line
;;     ellipsis `⋯` (U+22EF) rendered in amber (`my/hs-ellipsis-face') so it
;;     cannot be confused with YAML's own `...` document-end marker.
;;   • While a block is folded its header line is rendered in a distinct italic
;;     blue-grey face (`my/hs-folded-header-face') and returns to normal on expand.
;;   • Auto-installs the tree-sitter yaml grammar on first use if it is absent,
;;     and remaps `yaml-mode' → `yaml-ts-mode' so either activation path uses the
;;     tree-sitter variant.
;;
;; Entry point: `my/hs-yaml-ts-mode-setup' is added to `yaml-ts-mode-hook'.

;; Register yaml grammar source so treesit-install-language-grammar knows it,
;; then auto-install the grammar on first run if the shared library is missing.
(when (require 'treesit nil t)
  (add-to-list 'treesit-language-source-alist
               '(yaml "https://github.com/ikatyang/tree-sitter-yaml"))
  (unless (treesit-language-available-p 'yaml)
    (message "tree-sitter: installing yaml grammar…")
    (treesit-install-language-grammar 'yaml)))

(defun my/hs-forward-sexp-by-indent (&optional _arg)
  "Move point forward past the current indentation-based block.
Intended as a `forward-sexp-function' for `hs-special-modes-alist' entries in
modes where structure is expressed by indentation rather than delimiters.
Stops after the last non-blank content line of the block, so any trailing
blank lines between blocks are left outside the hidden region and stay visible.
Always advances at least to the next line so `hs-hide-all' never stalls."
  (let ((block-indent (current-indentation))
        last-content-end)
    (forward-line 1)
    (while (and (not (eobp))
                (or (looking-at-p "^[[:space:]]*$")
                    (> (current-indentation) block-indent)))
      (unless (looking-at-p "^[[:space:]]*$")
        (setq last-content-end (line-end-position)))
      (forward-line 1))
    (when last-content-end
      (goto-char (1+ last-content-end)))))

;; Format: (mode start-re end-re comment-start forward-sexp-fn)
;;   start-re  – matches the entire header line (so only the block body is hidden)
;;   end-re    – nil; the forward-sexp-fn handles block end detection
;;   comment   – YAML line-comment character
(defun my/hs-register-indent-mode (mode)
  "Register MODE in `hs-special-modes-alist' using indentation-based folding."
  (add-to-list 'hs-special-modes-alist
               `(,mode
                 ;; Only match true block headers: lines that end with ':'
                 ;; (optionally followed by spaces).  This excludes leaf lines
                 ;; like "  host: localhost" and comment lines, so no runtime
                 ;; filtering is needed in hs-set-up-overlay.
                 "^[[:space:]]*[^#\n][^\n]*:[[:space:]]*$"
                 nil
                 "#"  ; enable comment-block folding for consecutive # lines
                 my/hs-forward-sexp-by-indent)))

;; Face for the fold ellipsis — distinct from YAML's own "..." document-end marker.
;; Uses a unicode midline ellipsis (⋯) so it's also visually different in shape.
(defface my/hs-ellipsis-face
  '((((background dark))  :foreground "#f0a500" :weight bold)
    (((background light)) :foreground "#c07000" :weight bold))
  "Face for the hs-minor-mode fold ellipsis in indentation-based modes."
  :group 'hideshow)

;; Face applied to the header line of a folded block.
(defface my/hs-folded-header-face
  '((((background dark))  :foreground "#a0b0d0" :slant italic)
    (((background light)) :foreground "#4060a0" :slant italic))
  "Face for the visible header line of a folded block."
  :group 'hideshow)

;; When a body overlay is deleted (block expanded), also remove its header overlay.
(defun my/hs-cleanup-header-overlay (ov)
  (when (and (overlayp ov)
             (overlay-buffer ov)
             (eq 'code (overlay-get ov 'hs)))
    (when-let (h (overlay-get ov 'hs-header-ov))
      (when (overlay-buffer h)
        (delete-overlay h)))))
(advice-add 'delete-overlay :before #'my/hs-cleanup-header-overlay)

;; Register yaml-ts-mode (grammar is available: either already was, or just installed above).
(defun my/hs-yaml-ts-mode-setup ()
  "Enable hs-minor-mode for yaml-ts-mode buffers with:
- colored header line when folded
- a distinctly coloured clickable fold ellipsis (⋯)
- a newline after each ellipsis so collapsed blocks don't run together on one line."
  (hs-minor-mode 1)
  ;; hs-minor-mode added (hs . t) to buffer-invisibility-spec (auto-ellipsis).
  ;; Replace with plain 'hs so text is hidden without the default uncoloured '...',
  ;; letting us supply our own styled before-string instead.
  (remove-from-invisibility-spec '(hs . t))
  (add-to-invisibility-spec 'hs)
  (setq-local hs-set-up-overlay
              (lambda (ov)
                (let ((kind (overlay-get ov 'hs)))
                  (cond
                   ((eq kind 'code)
                    ;; Real block header: colored ⋯, italic header line, newline.
                    (overlay-put ov 'before-string
                                 (propertize "⋯" 'face 'my/hs-ellipsis-face))
                    (let* ((body-start  (overlay-start ov))
                           (header-start (save-excursion
                                           (goto-char body-start)
                                           (line-beginning-position)))
                           (header-ov   (make-overlay header-start body-start)))
                      (overlay-put header-ov 'face 'my/hs-folded-header-face)
                      (overlay-put header-ov 'hs   'header)
                      (overlay-put ov 'hs-header-ov header-ov))
                    (overlay-put ov 'after-string "\n"))
                   ((eq kind 'comment)
                    ;; Comment block: just the colored ⋯, no header recoloring.
                    (overlay-put ov 'before-string
                                 (propertize "⋯" 'face 'my/hs-ellipsis-face))))))))

(when (and (require 'treesit nil t)
           (treesit-language-available-p 'yaml))
  (my/hs-register-indent-mode 'yaml-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  ;; Remap yaml-mode → yaml-ts-mode so any activation path uses the ts variant.
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  (add-hook 'yaml-ts-mode-hook #'my/hs-yaml-ts-mode-setup))

