;; ediff.el — behavioral configuration for ediff
;;
;; Faces are NOT set here; they live in `custom-set-faces' in init.el because
;; the `user' custom theme (which custom-set-faces writes to) has the highest
;; priority and would override any face set in a regular config file.

;; Restore the window layout that was active before ediff was invoked.
(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook         #'my-restore-pre-ediff-winconfig)

(with-eval-after-load 'ediff
  ;; Control panel appears as a regular window inside the current frame,
  ;; not in a separate frame.  Prevents desktop/tiling WM disruption.
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Side-by-side comparison is easier to read than top-and-bottom.
  (setq ediff-split-window-function #'split-window-horizontally))

