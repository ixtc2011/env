;;;
;;; ix-pm.el - Customization to outline mode to help manage to do list.
;;;

(defface font-lock-outline-bullet
  `((t (:foreground "maroon" :bold t))) "" :group 'outline)
(defface font-lock-outline-text
  `((t (:foreground "black" :bold nil))) "" :group 'outline)
(defface font-lock-outline-1
  `((t (:foreground "navy" :bold t))) "" :group 'outline)
(defface font-lock-outline-2
  `((t (:foreground "blue" :bold t))) "" :group 'outline)
(defface font-lock-outline-3
  `((t (:foreground "cyan4" :bold t))) "" :group 'outline)
(defface font-lock-outline-4
  `((t (:foreground "cyan3" :bold t))) "" :group 'outline)
(defface font-lock-outline-todo-high
  `((t (:foreground "firebrick" :bold t))) "" :group 'outline)
(defface font-lock-outline-todo-low
  `((t (:foreground "firebrick" :bold nil))) "" :group 'outline)
(setq outline-font-lock-keywords
  '(
    ("^\\(\\*+\\)[ \t]*\\(.+\\)?[ \t]*$"
     (2 (let ((len (- (match-end 1) (match-beginning 1))))
	  (or (cdr (assq len '((1 . font-lock-outline-1)
			       (2 . font-lock-outline-2)
			       (3 . font-lock-outline-3)
			       (4 . font-lock-outline-4))))
	      font-lock-outline-4))
	nil t))
    ("^\\(- .*\\)$"
     (1 font-lock-outline-todo-high t))
    ("^\\(-- .*\\)$"
     (1 font-lock-outline-todo-low t))
    ("\\[\\([A-Z][A-Za-z]+\\)*[0-9]+\\]"
     (1 font-lock-outline-text)))
  )
(require 'outline)
(define-key outline-mode-map "\C-cd" 'hide-entry)
(define-key outline-mode-map "\C-cs" 'show-entry)
(define-key outline-mode-map "\C-cw" 'ix-insert-weekly)
(define-key outline-mode-map "\C-c " 'ix-toggle-todo)

(defun ix-insert-weekly ()
  (interactive)
  (let* ((day (string-to-int (time-stamp-strftime "%w")))
	 (mon-delta (+ (* (- 1 day) 3600 24) (* 7 3600 24)))
	 (fri-delta (+ (* (- 5 day) 3600 24) (* 7 3600 24)))
	 (now (current-time)))
    (insert (concat "* "
		    (time-stamp-strftime "%02m/%02d/%02y" (ix-time-add now mon-delta))
		    " - "
		    (time-stamp-strftime "%02m/%02d/%02y" (ix-time-add now fri-delta))
		    "\n\n"))))

(defun ix-toggle-todo ()
  (interactive)
  (let ((buffer-modified-p (buffer-modified-p)))
    (if (and (cond ((looking-at "\\+\\+")
		    (save-excursion
		      (delete-char 2)
		      (insert "--"))
		    t)
		   ((looking-at "\\+")
		    (save-excursion
		      (delete-char 1)
		      (insert "-"))
		    t)
		   ((looking-at "\\-\\-")
		    (save-excursion
		      (delete-char 2)
		      (insert "++"))
		    t)
		   ((looking-at "\\-")
		    (save-excursion
		      (delete-char 1)
		      (insert "+"))
		    t))
	     (not buffer-modified-p))
	(save-buffer 0))))

(defun ix-time-add (time secs)
  (let ((hisec (nth 0 time))
	(losec (nth 1 time))
	(micro (nth 2 time))
	carry)
    (setq losec (+ losec secs))
    (setq carry (/ losec 65536))
    (if (zerop carry)
	(list hisec losec micro)
      (list (+ hisec carry) (abs (% losec 65536)) micro))))

(defun ix-remove-done ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (and (> (point) (point-min))
		(not (looking-at "^* ")))
      (next-line -1))
    (next-line 1)
    (while (and (< (point) (point-max))
		(not (looking-at "^* ")))
      (if (looking-at "^+")
	  (kill-line 1)
	(next-line 1)))))

(defun ix-remove-todo ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (and (> (point) (point-min))
		(not (looking-at "^* ")))
      (next-line -1))
    (next-line 1)
    (while (and (< (point) (point-max))
		(not (looking-at "^* ")))
      (if (looking-at "^-")
	  (kill-line 1)
	(next-line 1)))))

;;;
;;; Enable extent in Xemacs for file:xyxyx so that it can be clicked on.
;;;

(if (not (eq ix-emacs-type 'xemacs))
    ()

(defvar ix-extent-list ())
(defun ix-enable-extents ()
  (interactive)
  (save-excursion
    (setq ix-extent-list nil)
    (message "Searching for extents.")
    (goto-char (point-min))
    (while (re-search-forward "file:[a-zA-Z0-9_\\.~/-]+" nil t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (extent (make-extent start end)))
	(setq ix-extent-list (cons extent ix-extent-list))
	(set-extent-property extent 'highlight t)))))

(defun ix-control-button-1 (event)
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (mouse-set-point event)
  (let ((extent (extent-at (point)))
	file-name
	frame)
    (sit-for 1)
    (if (not extent)
	(mouse-yank event)
      (setq file-name (buffer-substring (+ (extent-start-position extent) 5)
					(extent-end-position extent)))
      (if (member 'shift (event-modifiers event))
	  (find-file-other-frame file-name)
	(find-file file-name)))))

(global-set-key [(control button1)] 'ix-control-button-1)
)

;;;
;;; Enable extent in GNU Emacs for file:xyxyx so that it can be clicked on.
;;;

(if (not (eq ix-emacs-type 'gnuemacs))
    ()

(defun ix-enable-extents ()
  (interactive)
  (save-excursion
    (let ((buffer-modified-p (buffer-modified-p)))
      (goto-char (point-min))
      (while (re-search-forward "file:[a-zA-Z0-9_\\.~/-]+" nil t)
	(let ((start (match-beginning 0))
	      (end (match-end 0)))
	  (let ((map (make-sparse-keymap)))
	    ;(define-key map [mouse-2] 'ix-button-2)
	    (put-text-property start end 'file-name t)
	    (put-text-property start end 'mouse-face 'highlight)
	    ;(put-text-property start end 'keymap map)
	    )))
      (set-buffer-modified-p buffer-modified-p))))

(defun ix-double-button-1 (e)
  (interactive "e")
  (let (point-save
	begin
	end
	file-name)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end e))))
      (goto-char (posn-point (event-end e)))
      (cond ((get-text-property (point) 'file-name)
	     (setq point-save (point))
	     (while (and (< (point-min) (point))
			 (get-text-property (point) 'file-name))
	       (backward-char))
	     (setq begin (1+ (point)))
	     (setq begin (+ begin 5)) ; +5 to skip over "file:"
	     (goto-char point-save)
	     (while (and (> (point-max) (point))
			 (get-text-property (point) 'file-name))
	       (forward-char))
	     (setq end (point))
	     (if (not (< begin end))
		 (error "Can't parse file name"))
	     (setq file-name (buffer-substring begin end)))))
    (select-window (posn-window (event-end e)))
    (if file-name
	(find-file file-name))))
(global-set-key [double-mouse-1] 'ix-double-button-1)
)

(add-hook 'outline-mode-hook (lambda () (ix-enable-extents)))
(provide 'ix-pm)
