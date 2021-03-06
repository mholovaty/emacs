;; Emacs Initialization

;; Packages archive
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

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

;; Backup directory
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/auto-save"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Local info
(push "/usr/local/share/info" Info-directory-list)

;; History
(setq savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; desktop-save-mode
(setq desktop-restore-frames t)
(setq desktop-restore-in-current-display t)
(setq desktop-restore-forces-onscreen nil)

;; Syntax highlighting is on
(global-font-lock-mode 1)

;; Start-up screen is off
(setq inhibit-startup-message t)

;; Confirmation on exit
(setq confirm-kill-emacs 'y-or-n-p)

;; Scratch buffer is text-mode
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")

;; Show matching parens
(show-paren-mode 1)

;; linum
(setq linum-format "%d ")

;; Display current culumn number
(setq column-number-mode t)

;; CC modes
(setq-default c-basic-offset 4)

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

;; dired-mode
;; Hide dired details by default
(add-hook 'dired-mode-hook
	  (lambda () (dired-hide-details-mode 1)))
;; Sort directories first
(setq dired-listing-switches "-aBhl  --group-directories-first")

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

;; python-mode
(use-package python-mode
	     :mode ("\\.py\\'" . python-mode)
	     :init
	     (add-hook 'python-mode-hook #'elpy-mode)
	     (add-hook 'python-mode-hook #'elpy-enable)
	     (add-hook 'python-mode-hook #'hs-minor-mode))

;; disable python indent guessing
(setq python-indent-guess-indent-offset nil)

(use-package elpy
  :init
  (setq elpy-rpc-python-command "python3.6")
  (setq python-shell-interpreter "ipython3"
	python-shell-interpreter-args "-i --simple-prompt")
  ;; (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-modules '(elpy-module-company
  ;; 		       elpy-module-eldoc
  ;; 		       elpy-module-sane-defaults
  ;; 		       elpy-module-pyvenv))
  )

;; hs-minor-mode
(global-set-key (kbd "C-c [") 'hs-hide-block)
(global-set-key (kbd "C-c ]") 'hs-show-block)
(global-set-key (kbd "C-c -") 'hs-hide-all)
(global-set-key (kbd "C-c =") 'hs-show-all)
(global-set-key (kbd "C-c 0") 'hs-hide-level)

;; hard-code black background
(set-background-color "black")

;; custom generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-test-nose-runner-command (quote ("nosetests --nocapture --nologcapture")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(package-selected-packages
   (quote
    (xclip json-mode flymake-json flymake-jslint cmake-mode csv-mode vlf cmake-project neotree elpy paredit geiser markdown-mode magit dockerfile-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-A ((t (:background "brightblack"))))
 '(ediff-even-diff-B ((t (:background "brightblack"))))
 '(ediff-odd-diff-A ((t (:background "brightblack"))))
 '(ediff-odd-diff-B ((t (:background "brightblack")))))
