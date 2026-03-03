;; warnings.el
;; Prevent *Warnings* buffer from hijacking the window layout.
;; Warnings are still logged in *Warnings* (visit it with C-x b *Warnings*)
;; and echoed briefly in the minibuffer.

;; Never auto-display the *Warnings* buffer.
(add-to-list 'display-buffer-alist
             '("\\`\\*Warnings\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; Echo each warning in the minibuffer so it isn't silently swallowed.
(defun my/warning-to-echo (type message &optional level _buffer-name)
  (message "[%s %s] %s"
           (or level "warning")
           (if (listp type) (mapconcat #'symbol-name type "/") type)
           message))

(advice-add 'display-warning :before #'my/warning-to-echo)
