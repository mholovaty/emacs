;; Emacs Initialization

;; Packages archive
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

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

;; CC modes
(setq-default
 c-default-style "linux"
 c-basic-offset 4)

;; nxlm-mode
;; 4 space ident
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; sh-mode
;; space only ident
(add-hook 'sh-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; smerge-mode
;; Change from C-c ^ to C-c v
(setq smerge-command-prefix "\C-cv")

;; dired-mode
;; Hide dired details by default
(add-hook 'dired-mode-hook
	  (lambda () (dired-hide-details-mode 1)))

;; geiser
(setq geiser-active-implementations '(guile))

;; custom generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck paredit geiser markdown-mode magit dockerfile-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
