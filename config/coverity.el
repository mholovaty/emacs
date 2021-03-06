;;; coverity.el
;;; command and keybinding to invoke Coverity desktop analysis

;; Tested on:
;;   - emacs 20.7 on linux64
;;   - emacs 24.2 on linux64
;;   - cygwin emacs 24.3 on win64 with cygwin 1.7.24
;;   - cygwin emacs 23.2 on win32 with cygwin 1.5
;;   - NT emacs 24.0 on win32 with cygwin 1.5

;; Main analysis command.
(defun coverity ()
  "Run Coverity desktop analysis on current file.

This command expects cov-run-desktop to be in the PATH.  It invokes
that command using the current buffer's file name as an argument.

cov-run-desktop, in turn, expects a coverity.conf file to be in the
directory containing the current buffer's file, or in some ancestor
directory.

The console output of the script will be treated like compiler syntax
errors, which can be navigated using M-g n and M-g p."
  (interactive)

  (let ((bfn (buffer-file-name)))
    (if bfn
      ;; Set `compilation-buffer-name-function' so the resulting buffer
      ;; will not be called "*compilation*", and hence we can have both
      ;; a Coverity buffer and a normal compilation buffer.  This relies
      ;; on Lisp dynamic scoping since the call is inside `compile'.
      (let ((compilation-buffer-name-function
             (lambda (&rest ignored) "*coverity*")))

        ;; Invoke cov-run-desktop on the current file, using its
        ;; containing directory as the working directory.
        ;;
        ;; If you are using SCM integration, you may have to set
        ;; settings.scm.project_root in coverity.conf in order for
        ;; invocation from a subdirectory to work.
        (compile (concat "cov-run-desktop \"" (basename bfn) "\"")))

      (error "Current buffer does not have a file name.")
    )))


;; Make path and "supporting evidence" events "info"-level.
;;
;; In emacs 22 and later, such events will have their file name shown
;; in a different color, and by default are not traversed by M-g n and
;; M-g p.  See the `compilation-skip-threshold' variable.  This
;; addition has no effect in emacs versions prior to 22 (the final "0"
;; is interpreted differently, harmlessly).
;;
;; In the added element, "1 2 nil 0" means:
;;   1 = first match = FILE
;;   2 = second match = LINE
;;   nil = COLUMN
;;   0 = type = info
;;
;; This variable does not exist at all on some builds of cygwin emacs,
;; so we just skip modifying it in that case.  However, even when defined,
;; I can't get it to work on any Windows emacs (NT emacs or cygwin emacs);
;; I don't know why.
(if (boundp 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist
    (cons '("^\\(.+\\):\\([0-9]+\\):\\(\n  \\([0-9]+\\.\\)+ path:\\| Supporting evidence\\)" 1 2 nil 0)
          compilation-error-regexp-alist)))


;; Bind this command to Alt-F9.  (Doesn't seem to work for cygwin
;; emacs, so use "M-x coverity" instead.)
(global-set-key [M-f9] 'coverity)


(defun basename (path)
  "Return the filename part of a pathname."

  (let ((sep (string-match "[/\\][^/\\]+$" path)))
    (if sep
      (substring path (+ sep 1))       ; part after last separator
      (if (string-match "[/\\]" path)
        ""                             ; ends with a path separator
        path                           ; no separator
    ))))

;; Unit tests for basename.
(defun test-basename (input expect-basename)
  (let ((actual-basename (basename input)))
    (if (not (equal expect-basename actual-basename))
      (error (concat "Expected basename of \"" input
                     "\" to be \"" expect-basename
                     "\", but it was actually \"" actual-basename "\".")))
    "ok"
  ))

(progn
  (test-basename "a"      "a")
  (test-basename "a/"     "")
  (test-basename "a/b"    "b")
  (test-basename "a/b/"   "")
  (test-basename "a/b/c"  "c")
  (test-basename "/a/b"   "b")
  (test-basename "/a"     "a")
  (test-basename "/"      "")

  (test-basename "c:/a/b"   "b")
  (test-basename "c:/a"     "a")
  (test-basename "c:/"      "")

  (test-basename "c:\\a\\b"  "b")
  (test-basename "c:\\a"     "a")
  (test-basename "c:\\"      "")
)

;;; EOF
