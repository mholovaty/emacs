;;; minesweeper.el --- Minesweeper game for Emacs -*- lexical-binding: t -*-

;; Keywords: games

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup minesweeper nil
  "Minesweeper game."
  :group 'games)

(defcustom minesweeper-default-rows 16
  "Default number of rows."
  :type 'integer :group 'minesweeper)

(defcustom minesweeper-default-cols 30
  "Default number of columns."
  :type 'integer :group 'minesweeper)

(defcustom minesweeper-default-mines 99
  "Default number of mines."
  :type 'integer :group 'minesweeper)

;;; Faces

(defface minesweeper-hidden
  '((((background dark)) :background "gray35" :foreground "gray35")
    (t :background "gray70" :foreground "gray70"))
  "Face for unrevealed cells." :group 'minesweeper)

(defface minesweeper-revealed
  '((((background dark)) :background "gray22" :foreground "gray80")
    (t :background "gray85" :foreground "black"))
  "Face for revealed empty cells." :group 'minesweeper)

(defface minesweeper-flagged
  '((t :foreground "red" :background "gray35" :weight bold))
  "Face for flagged cells." :group 'minesweeper)

(defface minesweeper-exploded
  '((t :foreground "white" :background "red" :weight bold))
  "Face for the mine the player hit." :group 'minesweeper)

(defface minesweeper-mine
  '((((background dark)) :foreground "orange" :background "gray22" :weight bold)
    (t :foreground "red" :background "gray85" :weight bold))
  "Face for revealed mines after game over." :group 'minesweeper)

(defface minesweeper-1 '((t :foreground "#6699ff" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-2 '((t :foreground "#44bb44" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-3 '((t :foreground "#ff4444" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-4 '((t :foreground "#6666cc" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-5 '((t :foreground "#cc4444" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-6 '((t :foreground "#44aaaa" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-7 '((t :foreground "#cccccc" :background "gray22")) "" :group 'minesweeper)
(defface minesweeper-8 '((t :foreground "#888888" :background "gray22")) "" :group 'minesweeper)

;;; Buffer-local state

(defvar-local minesweeper--rows 16)
(defvar-local minesweeper--cols 30)
(defvar-local minesweeper--num-mines 99)
(defvar-local minesweeper--mines nil    "Bool vector: t if cell has a mine.")
(defvar-local minesweeper--revealed nil "Bool vector: t if cell is revealed.")
(defvar-local minesweeper--flagged nil  "Bool vector: t if cell is flagged.")
(defvar-local minesweeper--counts nil   "Integer vector: adjacent mine count.")
(defvar-local minesweeper--game-over nil)
(defvar-local minesweeper--won nil)
(defvar-local minesweeper--first-click t)
(defvar-local minesweeper--exploded-idx nil)
(defvar-local minesweeper--board-marker nil "Marker at the first board row.")

;;; Keymap

(defvar minesweeper-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")  #'minesweeper-reveal-at-point)
    (define-key map (kbd "SPC")  #'minesweeper-reveal-at-point)
    (define-key map (kbd "f")    #'minesweeper-flag-at-point)
    (define-key map (kbd "r")    #'minesweeper-restart)
    (define-key map (kbd "n")    #'minesweeper-new-game)
    (define-key map (kbd "q")    #'quit-window)
    (define-key map [mouse-1]    #'minesweeper-mouse-reveal)
    (define-key map [mouse-3]    #'minesweeper-mouse-flag)
    map)
  "Keymap for `minesweeper-mode'.")

(define-derived-mode minesweeper-mode special-mode "Minesweeper"
  "Major mode for playing Minesweeper.
\\{minesweeper-mode-map}"
  (setq-local truncate-lines t)
  (buffer-disable-undo))

;;; Core logic

(defun minesweeper--idx (row col)
  "Return linear index for ROW and COL."
  (+ (* row minesweeper--cols) col))

(defun minesweeper--neighbors (row col)
  "Return list of (row . col) for the up-to-8 neighbors of ROW, COL."
  (let (result)
    (dolist (dr '(-1 0 1))
      (dolist (dc '(-1 0 1))
        (unless (and (= dr 0) (= dc 0))
          (let ((r (+ row dr)) (c (+ col dc)))
            (when (and (>= r 0) (< r minesweeper--rows)
                       (>= c 0) (< c minesweeper--cols))
              (push (cons r c) result))))))
    result))

(defun minesweeper--place-mines (safe-row safe-col)
  "Randomly place mines, keeping SAFE-ROW SAFE-COL and its neighbors clear."
  (let ((safe (cons (cons safe-row safe-col)
                    (minesweeper--neighbors safe-row safe-col)))
        (total (* minesweeper--rows minesweeper--cols))
        (placed 0))
    (while (< placed minesweeper--num-mines)
      (let* ((idx (random total))
             (r (/ idx minesweeper--cols))
             (c (% idx minesweeper--cols)))
        (unless (or (aref minesweeper--mines idx)
                    (member (cons r c) safe))
          (aset minesweeper--mines idx t)
          (cl-incf placed))))))

(defun minesweeper--compute-counts ()
  "Fill `minesweeper--counts' with adjacent mine counts."
  (dotimes (r minesweeper--rows)
    (dotimes (c minesweeper--cols)
      (let ((idx (minesweeper--idx r c)))
        (unless (aref minesweeper--mines idx)
          (aset minesweeper--counts idx
                (cl-count-if (lambda (n)
                               (aref minesweeper--mines
                                     (minesweeper--idx (car n) (cdr n))))
                             (minesweeper--neighbors r c))))))))

(defun minesweeper--flood-reveal (row col)
  "Reveal ROW, COL; flood-fill outward if count is 0."
  (let ((idx (minesweeper--idx row col)))
    (unless (or (aref minesweeper--revealed idx)
                (aref minesweeper--flagged idx))
      (aset minesweeper--revealed idx t)
      (when (= (aref minesweeper--counts idx) 0)
        (dolist (n (minesweeper--neighbors row col))
          (minesweeper--flood-reveal (car n) (cdr n)))))))

(defun minesweeper--check-win ()
  "Return t when all non-mine cells are revealed."
  (= (cl-count t minesweeper--revealed)
     (- (* minesweeper--rows minesweeper--cols) minesweeper--num-mines)))

;;; Rendering

(defun minesweeper--cell-string (row col)
  "Return the propertized display string for cell at ROW, COL."
  (let* ((idx      (minesweeper--idx row col))
         (mine     (aref minesweeper--mines idx))
         (revealed (aref minesweeper--revealed idx))
         (flagged  (aref minesweeper--flagged idx))
         (count    (aref minesweeper--counts idx))
         (exploded (eql idx minesweeper--exploded-idx)))
    (cond
     (exploded
      (propertize " X " 'face 'minesweeper-exploded))
     ((and minesweeper--game-over (not minesweeper--won) mine (not flagged))
      (propertize " * " 'face 'minesweeper-mine))
     (flagged
      (propertize " F " 'face 'minesweeper-flagged))
     (revealed
      (if (= count 0)
          (propertize "   " 'face 'minesweeper-revealed)
        (propertize (format " %d " count)
                    'face (intern (format "minesweeper-%d" count)))))
     (t
      (propertize "   " 'face 'minesweeper-hidden)))))

(defun minesweeper--render ()
  "Redraw the entire board buffer."
  (let ((inhibit-read-only t)
        (saved-point (point)))
    (erase-buffer)
    ;; Header
    (insert (format " Minesweeper  |  Mines: %d   Flags: %d   Unflagged: %d\n\n"
                    minesweeper--num-mines
                    (cl-count t minesweeper--flagged)
                    (- minesweeper--num-mines (cl-count t minesweeper--flagged))))
    ;; Status
    (cond
     ((and minesweeper--game-over minesweeper--won)
      (insert (propertize " *** YOU WIN! Congratulations! Press 'r' to play again. ***\n\n"
                          'face '(:foreground "green" :weight bold))))
     (minesweeper--game-over
      (insert (propertize " *** BOOM! Game Over. Press 'r' to try again. ***\n\n"
                          'face '(:foreground "red" :weight bold)))))
    ;; Record where the board starts
    (set-marker minesweeper--board-marker (point))
    ;; Board rows
    (dotimes (r minesweeper--rows)
      (insert " ")
      (dotimes (c minesweeper--cols)
        (insert (minesweeper--cell-string r c)))
      (insert "\n"))
    ;; Footer
    (insert "\n RET/SPC: reveal   f: flag   r: restart   n: new game   q: quit\n")
    (goto-char (min saved-point (point-max)))))

;;; Point → cell conversion

(defun minesweeper--point-to-cell ()
  "Return (row . col) for the cell under point, or nil."
  (when minesweeper--board-marker
    (let* ((board-line (line-number-at-pos (marker-position minesweeper--board-marker)))
           (cur-line   (line-number-at-pos))
           (row        (- cur-line board-line))
           ;; Leading space + 3 chars per cell
           (col        (/ (max 0 (- (current-column) 1)) 3)))
      (when (and (>= row 0) (< row minesweeper--rows)
                 (>= col 0) (< col minesweeper--cols))
        (cons row col)))))

;;; Commands

(defun minesweeper-reveal-at-point ()
  "Reveal the cell at point."
  (interactive)
  (unless minesweeper--game-over
    (when-let ((cell (minesweeper--point-to-cell)))
      (let* ((row (car cell))
             (col (cdr cell))
             (idx (minesweeper--idx row col)))
        (when minesweeper--first-click
          (setq minesweeper--first-click nil)
          (minesweeper--place-mines row col)
          (minesweeper--compute-counts))
        (unless (or (aref minesweeper--revealed idx)
                    (aref minesweeper--flagged idx))
          (if (aref minesweeper--mines idx)
              (progn
                (setq minesweeper--exploded-idx idx)
                (aset minesweeper--revealed idx t)
                (setq minesweeper--game-over t))
            (minesweeper--flood-reveal row col)
            (when (minesweeper--check-win)
              (setq minesweeper--game-over t minesweeper--won t)))
          (minesweeper--render))))))

(defun minesweeper-flag-at-point ()
  "Toggle a flag on the cell at point."
  (interactive)
  (unless minesweeper--game-over
    (when-let ((cell (minesweeper--point-to-cell)))
      (let* ((row (car cell))
             (col (cdr cell))
             (idx (minesweeper--idx row col)))
        (unless (aref minesweeper--revealed idx)
          (aset minesweeper--flagged idx (not (aref minesweeper--flagged idx)))
          (minesweeper--render))))))

(defun minesweeper-mouse-reveal (event)
  "Reveal the cell at mouse EVENT position."
  (interactive "e")
  (mouse-set-point event)
  (minesweeper-reveal-at-point))

(defun minesweeper-mouse-flag (event)
  "Flag the cell at mouse EVENT position."
  (interactive "e")
  (mouse-set-point event)
  (minesweeper-flag-at-point))

;;; Initialization

(defun minesweeper--init (rows cols mines)
  "Set up a fresh game with ROWS rows, COLS columns, and MINES mines."
  (let ((total (* rows cols)))
    (setq minesweeper--rows        rows
          minesweeper--cols        cols
          minesweeper--num-mines   mines
          minesweeper--mines       (make-vector total nil)
          minesweeper--revealed    (make-vector total nil)
          minesweeper--flagged     (make-vector total nil)
          minesweeper--counts      (make-vector total 0)
          minesweeper--game-over   nil
          minesweeper--won         nil
          minesweeper--first-click t
          minesweeper--exploded-idx nil)
    (unless minesweeper--board-marker
      (setq minesweeper--board-marker (make-marker)))))

(defun minesweeper-restart ()
  "Start a new game with the same dimensions and mine count."
  (interactive)
  (minesweeper--init minesweeper--rows minesweeper--cols minesweeper--num-mines)
  (minesweeper--render))

(defun minesweeper-new-game ()
  "Start a new game, prompting for difficulty."
  (interactive)
  (let* ((choices '("Beginner    (9x9,   10 mines)"
                    "Intermediate (16x16, 40 mines)"
                    "Expert       (16x30, 99 mines)"
                    "Custom"))
         (choice (completing-read "Difficulty: " choices nil t nil nil
                                  "Expert       (16x30, 99 mines)"))
         rows cols mines)
    (cond
     ((string-prefix-p "Beginner"     choice) (setq rows 9  cols 9  mines 10))
     ((string-prefix-p "Intermediate" choice) (setq rows 16 cols 16 mines 40))
     ((string-prefix-p "Expert"       choice) (setq rows 16 cols 30 mines 99))
     (t
      (setq rows  (read-number "Rows: "   16)
            cols  (read-number "Cols: "   30)
            mines (read-number "Mines: "  99))
      (setq mines (min mines (- (* rows cols) 9)))))
    (minesweeper--init rows cols mines)
    (minesweeper--render)))

;;;###autoload
(defun minesweeper ()
  "Play Minesweeper."
  (interactive)
  (let ((buf (get-buffer-create "*Minesweeper*")))
    (switch-to-buffer buf)
    (unless (derived-mode-p 'minesweeper-mode)
      (minesweeper-mode))
    (minesweeper--init minesweeper-default-rows
                       minesweeper-default-cols
                       minesweeper-default-mines)
    (minesweeper--render)))

(provide 'minesweeper)
;;; minesweeper.el ends here
