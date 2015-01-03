;;;
;;; compatibility stuff
;;;
(if (fboundp 'frame-height)
    ()

(defun frame-height ()
  (screen-height))
)

(if (fboundp 'window-minibuffer-p)
    ()

(defun window-minibuffer-p (win)
  (eq win (minibuffer-window)))
)

(fset 'delete-window-balanced "xdelete-windowxbalance-windows")
(global-set-key "\C-X0" 'delete-window-balanced)
(fset 'split-window-balanced "xsplit-window-verticallyxbalance-windows")
(global-set-key "\C-X2" 'split-window-balanced)

(defun balance-windows ()
  "Makes all visible windows the same size."
  (interactive)
  (let ((size (/ (frame-height) (count-windows))))
    (walk-windows (function (lambda (&optional window)
			      (enlarge-window (- size (window-height)))))
		  'no-mini)))

;;;
;;; Redefined from subr.el.
;;;
(defun walk-windows (proc &optional minibuf all-frames)
  "Cycle through all visible windows, calling PROC for each one.
PROC is called with a window as argument.
Optional second arg MINIBUF t means count the minibuffer window
even if not active.  If MINIBUF is neither t nor nil it means
not to count the minibuffer even if it is active.

Optional third arg ALL-FRAMES, if t, means include all frames.
ALL-FRAMES nil or omitted means cycle within the selected frame,
but include the minibuffer window (if MINIBUF says so) that that
frame uses, even if it is on another frame.
If ALL-FRAMES is neither nil nor t, stick strictly to the selected frame."
  ;; If we start from the minibuffer window, don't fail to come back to it.
  (if (window-minibuffer-p (selected-window))
      (setq minibuf t))
  (let* ((start (selected-window))
	 (current start)
	 (done nil))
    (while (not done)
      (select-window current)
      (funcall proc current)
      (setq current (next-window current minibuf all-frames))
      (setq done (eq current start)))
    (select-window start)))

(defun count-windows (&optional no-mini)
   "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
   (let ((count 0))
     (walk-windows (function (lambda (&optional window)
			       (setq count (+ count 1))))
		   no-mini)
     count))
