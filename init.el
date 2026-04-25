;; Emacs Initialization

;; (setq package-check-signature nil)

;; Packages archive
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Auto-install missing packages
(setq use-package-always-ensure t)
(unless package-archive-contents
  (package-refresh-contents))

;; Auto-load configuration
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
	   (fullpath (concat directory "/" path))
	   (isdir (car (cdr element)))
	   (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
	(load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
	(load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/config")

;; Run server
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Invoking-emacsclient.html
(server-start)

;; Mute sound
(setq ring-bell-function 'ignore)

;; Suppress debug-level warnings (e.g. flymake diagnostics spam)
(setq warning-minimum-log-level :warning)

;; Windmove alternative keys (on top of shift+arrows)
(global-set-key (kbd "C-,") 'windmove-left)
(global-set-key (kbd "C-.") 'windmove-right)
(global-set-key (kbd "M-p") 'windmove-up)
(global-set-key (kbd "M-n") 'windmove-down)

;; Enable mouse and copy&paste if in terminal
(when (not (display-graphic-p))
  (xterm-mouse-mode t)
  (xclip-mode t)
  (global-company-mode -1)
  (global-eldoc-mode -1)
  (add-hook 'eglot-managed-mode-hook (lambda () (eldoc-mode -1))))

;; Enable full color support if env variable is set
;; (when (string= (getenv "COLORTERM") "truecolor")
;;   (set-terminal-coding-system 'utf-8))

;; Disable tool bar
(tool-bar-mode -1)

;; Backup directory
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/auto-save"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Disable lockfiles '.#*'
(setq create-lockfiles nil)

;; Local info
(push "/usr/local/share/info" Info-directory-list)

;; History
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode t)

;; desktop-save-mode
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

;; persp-mode
(use-package persp-mode
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  :config
  (persp-mode t))

;; Syntax highlighting is on
(global-font-lock-mode t)

;; Start-up screen is off
(setq inhibit-startup-message t)

;; Confirmation on exit
(setq confirm-kill-emacs 'y-or-n-p)

;; Scratch buffer is text-mode
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

;; windmove keybingings (shirt + arrows)
(windmove-default-keybindings)

;; Show matching parens
(show-paren-mode t)

;; multiple-cursors
(use-package multiple-cursors :demand t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; linum
(setq linum-format "%d ")

;; Display current culumn number
(setq column-number-mode t)

;; Delete trailing whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-vc-integration '(face char)
        neo-vc-ignore-messages t
	neo-smart-open t ;; do not jump to subtree
        neo-show-hidden-files nil))
;; (add-hook 'after-save-hook #'neotree-refresh)
;; (remove-hook 'after-save-hook #'neotree-refresh)
(setq neo-hidden-regexp-list
      '("^\\.git$"
        "^\\.mypy_cache$"
        "^__pycache__$"
        "^build$"
        "^\\.pytest_cache$"
	"^\\.ruff_cache/"
	"\\.egg-info/?$"))
(global-set-key (kbd "C-c t") 'neotree-toggle)

;; dasht — offline documentation browser
;; Setup:
;;   git clone https://github.com/sunaku/dasht ~/.local/share/dasht
;;   ln -sf ~/.local/share/dasht/bin/dasht* ~/.local/bin/
;;   sudo apt install -y sqlite3 w3m
;;   dasht-docsets-install C
;;   dasht-docsets-install 'C++'
;;   dasht-docsets-install Python
;;   dasht-docsets-install Go
;;
;; Usage:
;;   C-c D     — pick match from list, open doc in eww buffer
;;   C-u C-c D — prompt for custom query
(defun my/dasht-at-point (arg)
  "Look up symbol at point in dasht. With prefix ARG, prompt for query.
Presents matches via completing-read, then opens the selected doc in eww."
  (interactive "P")
  (let* ((query (if arg
                    (read-string "dasht query: ")
                  (thing-at-point 'symbol t)))
         (output (shell-command-to-string
                  (format "dasht-query-line %s" (shell-quote-argument query))))
         (lines (seq-filter #'identity (split-string output "\n")))
         (choice (completing-read (format "dasht [%s]: " query) lines nil t))
         (url (nth 3 (split-string choice "\t"))))
    (when url
      (eww url))))

(global-set-key (kbd "C-c D") #'my/dasht-at-point)

;; vterm
(use-package vterm
  :demand t
  :config
  ;; Fix ANSI blue not to be too dark
  (set-face-foreground 'vterm-color-blue "#5fafff")
  ;; Fallback font for missing glyphs (e.g. Claude Code TUI uses U+23BF)
  ;; sudo apt install -y fonts-unifont && fc-cache -f
  (set-fontset-font t nil "Unifont" nil 'append))

;; CC modes
(setq c-default-style '((c-mode    . "linux")
                         (c++-mode  . "k&r")
                         (other     . "linux")))
(setq-default c-basic-offset 4)

;; Ensure .cpp/.hpp always open in c++-mode
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'"  . c++-mode))

(defun my-c-ff-find-other-file ()
  (local-set-key (kbd "C-c C-f") #'ff-find-other-file))

(add-hook 'c-mode-hook #'my-c-ff-find-other-file)
(add-hook 'c++-mode-hook #'my-c-ff-find-other-file)

;; Don't let eglot log unboundedly — a full log buffer itself causes slowdowns
(setq eglot-events-buffer-size 0)

;; Throttle didChange notifications — don't send on every keystroke
(setq eglot-send-changes-idle-time 0.5)

;; clangd integration
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'c++-mode)
              (add-hook 'before-save-hook #'eglot-format-buffer nil t))))


;; Copilot
;; curl -fsSL https://gh.io/copilot-install | bash
;; sudo sudo npm install -g @github/copilot-language-server
(defun my/copilot-accept-or-eol ()
  "Move to end of line, then accept Copilot completion if available."
  (interactive)
  (end-of-line)
  (when (copilot--overlay-visible)
    (copilot-accept-completion)))
(global-set-key (kbd "C-c e") 'copilot-mode)
(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "C-e") #'my/copilot-accept-or-eol))
;; Set fallback indent to 4 spaces
(setq copilot-indent-offset 4)

;; nxml-mode
;; 4 space ident
(setq nxml-child-indent 4 nxml-attribute-indent 4)
;; Disable validation as workaround to memory leaking
(setq rng-nxml-auto-validate-flag nil)

;; sh-mode
;; space only ident
(add-hook 'sh-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; smerge-mode
;; Change from C-c ^ to C-c v
(setq smerge-command-prefix "\C-cv")

;; magit
;; show 2 ediff windows, not 3
(setq magit-ediff-dwim-show-on-hunks t)

;; Diffs highlight
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; dired-mode
;; Hide dired details by default
(add-hook 'dired-mode-hook
	  (lambda () (dired-hide-details-mode t)))
;; Sort directories first
(setq dired-listing-switches "-aBhl  --group-directories-first")
;; Hide files listed in .gitignore
(use-package dired-gitignore
  :ensure t
  :hook (dired-mode . dired-gitignore-mode))

;; Auto-revert all buffers on externa files change
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)  ;; no "Reverting buffer..." messages
(global-auto-revert-mode t)

;; Prevent *Buffer List* from auto-reverting
(defun my/no-auto-revert-buffer-list ()
  (when (string= (buffer-name) "*Buffer List*")
    (setq-local buffer-stale-function #'ignore)))

(add-hook 'buffer-list-update-hook #'my/no-auto-revert-buffer-list)

;; geiser
(setq geiser-active-implementations '(guile))

;; shell mode fix
(defun my-resize-window ()
  "Reset the COLUMNS environment variable to the current width of the window."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(str (format "export COLUMNS=%s" (window-width))))
    (funcall comint-input-sender proc str)))

(defun my-shell-mode-hook ()
  (local-set-key "\C-cw" 'my-resize-window))

;; Accept project variables
(setq enable-local-variables :safe)

;; Accept .dir-locals.el
;; ((dired-mode
;;  (dired-listing-switches . "-alh --group-directories-first -t")))
(put 'dired-listing-switches 'safe-local-variable #'stringp)

;; Enable side-by-side 'C' copy in dired
(setq dired-dwim-target t)

;; lsp-mode
(use-package lsp-mode :demand t)

;; go-mode, lsp-mode, lsp-ui
;; go install golang.org/x/tools/gopls@latest
;; go install github.com/go-delve/delve/cmd/dlv@latest

;; yasnippet
(use-package yasnippet :demand t)

(use-package dape :demand t)
;; Mark dape-configs as safe so Emacs won't ask
(put 'dape-configs 'safe-local-variable #'listp)

;; go-mode
(add-hook
 'go-mode-hook
 (lambda ()
   ;; Start LSP
   (lsp-deferred)
   ;; Set tab width to 4
   (setq tab-width 4)
   ;; Keep Go’s convention of using tabs
   (setq indent-tabs-mode t)
   ;; yasnippet
   (yas-minor-mode)
   (hs-minor-mode)))

;; Format and organize imports on save
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional UI enhancements
;;(require 'lsp-ui)
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-enable-folding t))


;; Python related settings

;; disable python indent guessing
(setq python-indent-guess-indent-offset nil)

(use-package elpy
  :init
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "ipython3"
	python-shell-interpreter-args "-i --simple-prompt")
  ;; (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-modules '(elpy-module-company
  ;; 		       elpy-module-eldoc
  ;; 		       elpy-module-sane-defaults
  ;; 		       elpy-module-pyvenv))
  )

;; eglot + python
;; pip install pyright ruff
;; There might be ruff-lsp version incompatibility.
;;   pip3 install --user --break-system-packages \
;;      "ruff-lsp" \
;;      "lsprotocol==2023.0.0" \
;;      "ruff"
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background "#1c4a6e" :bold nil)
  (keymap-set eglot-mode-map "C-c a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c f" #'eglot-format))

;; Start Eglot automatically when entering python-mode
(add-hook 'python-mode-hook #'eglot-ensure)

;; Format with ruff on save (pyright doesn't provide document formatting)
(defun my/ruff-format-buffer ()
  "Format current Python buffer with ruff."
  (interactive)
  (when (and buffer-file-name (executable-find "ruff"))
    (let* ((orig (buffer-string))
           (formatted
            (with-temp-buffer
              (insert orig)
              (when (zerop (shell-command-on-region
                            (point-min) (point-max)
                            "ruff format --quiet -"
                            (current-buffer) t "*ruff-errors*"))
                (buffer-string)))))
      (when (and formatted (not (string= orig formatted)))
        (let ((p (point)))
          (erase-buffer)
          (insert formatted)
          (goto-char (min p (point-max))))))))

;; Make sure Emacs recognizes project roots
(setq project-vc-extra-root-markers '("pyproject.toml" "setup.py" "requirements.txt"))

;; Python Debug server
;; pip install debugpy
(use-package dap-mode
  :commands dap-debug

  :bind
  (("C-c d" . dap-debug)
   ("C-c k" . dap-disconnect)
   ("C-c b" . dap-breakpoint-toggle)
   ("C-c n" . dap-next)
   ("C-c i" . dap-step-in)
   ("C-c o" . dap-step-out)
   ("C-c c" . dap-continue))

  :config
  (dap-auto-configure-mode)

  ;; Core UI
  (dap-ui-mode t)
  (dap-tooltip-mode t)
  (dap-ui-controls-mode t)

  ;; Optional: inline variable overlays tuning
  (setq dap-auto-show-output t
        dap-ui-variable-length 50
        dap-ui-locals-expand-depth 3))

(use-package dap-python
  :ensure nil  ;; bundled with dap-mode, not a separate package
  :after dap-mode
  :config
  (setq dap-python-debugger 'debugpy
	dap-python-executable "ipython3"))

;; hs-minor-mode
(global-set-key (kbd "C-c [") 'hs-hide-all)
(global-set-key (kbd "C-c ]") 'hs-show-all)
(global-set-key (kbd "C-c l") 'hs-hide-level)
(global-set-key (kbd "C-c h") 'hs-toggle-hiding)

;; python-mode
(use-package python-mode
	     :mode ("\\.py\\'" . python-mode)
	     :init
	     (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook #'my/ruff-format-buffer nil t)))
	     ;; (add-hook 'python-mode-hook #'elpy-mode)
	     ;; (add-hook 'python-mode-hook #'elpy-enable)
	     (add-hook 'python-mode-hook #'hs-minor-mode))

;; (remove-hook 'python-mode-hook #'elpy-mode)
;; (remove-hook 'python-mode-hook #'elpy-enable)

;; hard-code black background
;; (set-background-color "black")

;; custom generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-dark))
 '(elpy-test-nose-runner-command '("nosetests --nocapture --nologcapture"))
 '(elpy-test-runner 'elpy-test-pytest-runner)
 '(package-selected-packages
   '(w3m dasht outshine diff-hl yaml-mode free-keys which-key dired-gitignore clang-format multiple-cursors yasnippet-snippets imenu-list eglot meson-mode copilot dap-mode dape persp-mode lsp-ui lsp-mode go-mode elpy gdscript-mode gnu-elpa-keyring-update ztree xclip json-mode flymake-json flymake-jslint cmake-mode csv-mode vlf cmake-project neotree paredit geiser markdown-mode magit dockerfile-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "#3B3B6B" :foreground "#E0E0FF"))))
 '(ediff-current-diff-B ((t (:background "#1E4620" :foreground "#C8E6C9"))))
 '(ediff-even-diff-A ((t (:background "#2D2D52" :foreground "#B0B0D8"))))
 '(ediff-even-diff-B ((t (:background "#1B3520" :foreground "#A0C8A0"))))
 '(ediff-fine-diff-A ((t (:background "#7A5800" :foreground "#FFF0A0"))))
 '(ediff-fine-diff-B ((t (:background "#005050" :foreground "#A0F0F0"))))
 '(ediff-odd-diff-A ((t (:background "#2D2D52" :foreground "#B0B0D8"))))
 '(ediff-odd-diff-B ((t (:background "#1B3520" :foreground "#A0C8A0"))))
 '(region ((t (:background "#555753" :foreground "#ffffff")))))
