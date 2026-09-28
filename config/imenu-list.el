;; imenu-list.el
;; Work around a stale-buffer bug in imenu-list (MELPA 20210420).
;;
;; `imenu-list-update' skips rescanning when point "didn't move", but it
;; compares markers with `=', i.e. by position only, ignoring which buffer
;; they belong to.  A freshly opened file at the same position as the last
;; tracked one is therefore never scanned, and `imenu-list--displayed-buffer'
;; can keep pointing at a killed buffer.  RET in *Ilist* then fails with
;; "display-buffer-assq-regexp: Wrong type argument: stringp, nil".
;;
;; The same early exit also skips its own "was *Ilist* killed?" check, so
;; re-opening imenu-list after killing *Ilist* shows an empty buffer.

(defun my/imenu-list-forget-location-on-buffer-change (&rest _)
  "Force a rescan on buffer change or when *Ilist* has been killed."
  (unless (and (eq (current-buffer) imenu-list--displayed-buffer)
               (get-buffer imenu-list-buffer-name))
    (setq imenu-list--last-location nil)))

;; RET (and mouse click) on a parent (class/function with children) jumps
;; to it instead of folding; SPC, TAB and f still fold.  Entries are
;; buttons whose own RET wins over the mode map, so the parent button
;; action is overridden too.

(defun my/imenu-list--parent-position (entry)
  "Position of parent ENTRY itself, or nil if unknown.
Eglot keeps it in the name's `breadcrumb-region'; python-mode adds a
\"*class definition*\" child, which is the earliest of the leaves."
  (or (car (get-text-property 0 'breadcrumb-region (car entry)))
      (let ((leaves (seq-remove #'imenu--subalist-p (cdr entry))))
        (cdr (car (seq-sort-by #'cdr #'< leaves))))))

(defun my/imenu-list-ret-jump ()
  "Jump to the entry at point, parents included; fold only as a fallback."
  (interactive)
  (let* ((entry (imenu-list--find-entry))
         (pos (and (imenu--subalist-p entry)
                   (my/imenu-list--parent-position entry))))
    (cond (pos (imenu-list--goto-entry (cons (car entry) pos)))
          ((imenu--subalist-p entry) (hs-toggle-hiding))
          (t (imenu-list--goto-entry entry)))))

(defun my/imenu-list--action-jump (event)
  "Button action for parent entries: jump via `my/imenu-list-ret-jump'.
EVENT is a mouse event on click, or the button itself on RET."
  (if (not (consp event))
      (my/imenu-list-ret-jump)
    (let ((window (posn-window (event-end event)))
          (ilist-buffer (get-buffer imenu-list-buffer-name)))
      (when (and (windowp window)
                 (eq (window-buffer window) ilist-buffer))
        (with-current-buffer ilist-buffer
          (goto-char (posn-point (event-end event)))
          (my/imenu-list-ret-jump))))))

(with-eval-after-load 'imenu-list
  (advice-add 'imenu-list--action-toggle-hs :override
              #'my/imenu-list--action-jump)
  (advice-add 'imenu-list-update :before
              #'my/imenu-list-forget-location-on-buffer-change)
  (keymap-set imenu-list-major-mode-map "RET" #'my/imenu-list-ret-jump))
