;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Time-stamp: <01/08/15 16:16:08 Daddy>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Unified init file that works for GNU emacs or XEmacs on Windows, Cygwin
;;; or Linux.
;;;

(defconst ix-load-path
  `((windows ,(concat "C:\\Cygwin\\Home\\" (user-login-name) "\\emacs\\"))
    (cygwin ,(expand-file-name "~/emacs"))
    (linux ,(expand-file-name "~/emacs"))))

(defconst ix-emacs-type
  (if (string-match "^GNU Emacs" (emacs-version))
      'gnuemacs
    'xemacs))

(defconst ix-system-type
  (let ((system-type-name (and (boundp 'system-type) (symbol-name system-type))))
    (cond ((null system-type-name)
	   nil)
	  ((string-match ".*linux.*" system-type-name)
	   'linux)
	  ((string-match ".*nt.*" system-type-name)
	   'windows)
	  ((string-match "cygwin" system-type-name)
	   'cygwin)
	  (t
	   nil)))
  "System type: Unix, Windows or Cygwin.")

(let ((load-path-extra (assoc ix-system-type ix-load-path)))
  (if load-path-extra
      (setq load-path (append (cdr load-path-extra) load-path))))

(setq default-major-mode 'indented-text-mode
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message nil
      inhibit-startup-message t
      auto-save-default nil
      make-backup-files nil
      enable-local-eval t
      require-final-newline nil
      next-line-add-newlines nil
      scroll-step 1
      next-screen-context-lines 1
      kill-whole-line nil
      kill-ring-max 100
)
(setq-default default-fill-column 75)
(custom-set-faces)

(cond ((eq ix-emacs-type 'xemacs)
       (set-specifier default-toolbar-visible-p nil)
       (setq toolbar-visible-p nil)
       (setq bar-cursor nil)
       (setq load-home-init-file t)
       (cond ((and (= emacs-major-version 21)
		   (= emacs-minor-version 4))
	      (set-specifier default-gutter-visible-p nil)
	      (custom-set-variables '(gutter-buffers-tab-visible-p nil))
	      (custom-set-variables '(default-gutter-position 'top))
	      (setq progress-feedback-use-echo-area t)
	      ))
       (setq frame-title-format (concat "%S @ " (system-name))
	     teach-extended-commands-p nil
	     zmacs-regions nil
	     minibuffer-max-depth nil
	     )
       )
      ((eq ix-emacs-type 'gnuemacs)
       (set-scroll-bar-mode 'right)
       (tool-bar-mode -1)
       (setq frame-title-format (concat "%F @ " (system-name))
	     eval-expression-print-length nil
	     eval-expression-print-level nil)
       (put 'upcase-region 'disabled nil)
       (put 'downcase-region 'disabled nil)
       ))

(if (fboundp 'line-number-mode)
    (line-number-mode 1))
(if (fboundp 'column-number-mode)
    (column-number-mode 1))

(defun ix-toggle-file-encoding ()
  (interactive)
  (cond ((eq buffer-file-coding-system 'undecided-unix)
	 (setq buffer-file-coding-system 'undecided-dos)
	 (message "File coding changed to undecided-dos"))
	(t
	 (setq buffer-file-coding-system 'undecided-unix)
	 (message "File coding changed to undecided-unix"))))
(global-set-key "\C-ce" 'ix-toggle-file-encoding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; The following customization are for external packages that get loaded.
;;;

(load "bal-wins" t)
(if (not (eq ix-system-type 'cygwin))
    ()
  ;;
  ;; This is to handle the case where a file is opened from Windows
  ;; using the Cygwin version of emacs.
  ;;
  (if (require 'windows-path nil t)
      (windows-path-activate))
)

(if (eq ix-system-type 'windows)
    ()
  (require 'vc)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (remove-hook 'find-file-not-found-hooks 'vc-file-not-found-hook)
  )

(require 'outline)
(if (eq ix-emacs-type 'gnuemacs)
    (setq outline-blank-line t)
  )

(require 'time-stamp)
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%02m/%02d/%02y %02H:%02M:%02S %u")
(if (eq ix-emacs-type 'gnuemacs)
    (defun time-stamp-strftime (x &optional y) (format-time-string x y))
  )

(defun ix-file-info ()
  (interactive)
  (let* ((file-name (buffer-file-name (current-buffer)))
	 (file-attributes (and file-name (file-attributes file-name))))
    (if (not file-attributes)
	(message "No file associated with the current buffer.")
      (let ((user (user-login-name (nth 2 file-attributes)))
	    (mtime (format-time-string "%D %r" (nth 5 file-attributes)))
	    (size (nth 7 file-attributes))
	    (mode (nth 8 file-attributes)))
	(message (format "%s, %s, %d bytes, %s" user mtime size mode))))))

;;;
;;; Misc custom key bindings.
;;;

(define-key global-map "\C-ci" 'ix-file-info)
(if (eq ix-system-type 'windows)
    (define-key global-map "\C-ct" (function (lambda () (interactive) (insert (time-stamp-strftime "%02m/%02d/%y")))))
  (define-key global-map "\C-ct" (function (lambda () (interactive) (insert (time-stamp-strftime "%02m/%02d/%Y")))))
  )
(fmakunbound 'rmail)
(global-set-key "\C-xn" 'other-window)
(global-set-key "\C-xp" (function (lambda () (interactive) (other-window -1))))
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cm" 'manual-entry)
(global-set-key "\C-c-" '(lambda () (interactive) (set-buffer-modified-p nil)))
(global-set-key "\C-c+" '(lambda () (interactive) (set-buffer-modified-p t)))
(global-set-key "\C-co" 'open-rectangle)
(global-set-key "\C-x\C-q" 'toggle-read-only)

;;;
;;; Assumes the following programs can be found in the default PATH:
;;;
;;;   aspell
;;;   bash
;;;   diff
;;;
;;; Otherwise exec-path should be extended.
;;;

(setq shell-file-name (executable-find "bash"))
(require 'ispell)
(setq ispell-program-name (executable-find "aspell"))

;;;
;;; Set up interactive diff.
;;;
(progn
  (require 'ediff)
  (if (not (executable-find "diff"))
      (warn "diff is needed to setup ediff"))
  (if (eq system-type 'windows-nt)
      (setq ediff-temp-file-prefix (concat (expand-file-name "~") "/temp/"))
    (setq ediff-temp-file-prefix "/tmp/"))
  (setq ediff-same-fg "firebrick")
  (setq ediff-same-bg "pale green")
  (setq ediff-diff-fg "black")
  (setq ediff-diff-bg "sky blue")
  (setq ediff-other-fg "black")
  (setq ediff-other-bg "light gray")
  (set-face-foreground ediff-current-diff-face-A ediff-same-fg)
  (set-face-background ediff-current-diff-face-A ediff-same-bg)
  (set-face-foreground ediff-current-diff-face-B ediff-same-fg)
  (set-face-background ediff-current-diff-face-B ediff-same-bg)
  (set-face-foreground ediff-fine-diff-face-A ediff-diff-fg)
  (set-face-background ediff-fine-diff-face-A ediff-diff-bg)
  (set-face-foreground ediff-fine-diff-face-B ediff-diff-fg)
  (set-face-background ediff-fine-diff-face-B ediff-diff-bg)
  (set-face-foreground ediff-even-diff-face-A ediff-other-fg)
  (set-face-background ediff-even-diff-face-A ediff-other-bg)
  (set-face-foreground ediff-even-diff-face-B ediff-other-fg)
  (set-face-background ediff-even-diff-face-B ediff-other-bg)
  (set-face-foreground ediff-odd-diff-face-A ediff-other-fg)
  (set-face-background ediff-odd-diff-face-A ediff-other-bg)
  (set-face-foreground ediff-odd-diff-face-B ediff-other-fg)
  (set-face-background ediff-odd-diff-face-B ediff-other-bg)
   ;; this fixes the hanging problem on using it on non-sunos.5.5 machine
  (defun ediff-has-toolbar-support-p () nil)
  ) ;; ediff setup

;;;
;;; Shell customizations.
;;;

(require 'shell)
(if (eq ix-emacs-type 'xemacs)
    (setq shell-multiple-shells nil))
(setq comint-password-prompt-regexp "^Password\\|^Enter password")
(if (eq ix-system-type 'unix)
    (progn
;      (setq explicit-shell-file-name "/bin/bash")
;      (setq comint-process-echoes t)
;      (setq comint-output-filter-functions
;	    (cons 'comint-strip-ctrl-m comint-output-filter-functions))
      ))

(defun ix-shell (name)
  (interactive "sName of buffer (*shell*): ")
  (let ((old-shell "*shell*"))
    (if (string= name "")
	(setq name "*shell*"))
    (cond ((get-buffer name)
	   (if (string= name old-shell)
	       (switch-to-buffer old-shell)
	     (error (concat "Buffer named " name " already exists."))))
	  (t
	   (unwind-protect
	       (progn
		 (cond ((get-buffer old-shell)
			(with-current-buffer (get-buffer old-shell)
			  (setq old-shell "*shell-old*")
			  (rename-buffer old-shell)))
		       (t
			(setq old-shell nil)))
		 (call-interactively 'shell)
		 (rename-buffer name))
	     (if old-shell
		 (with-current-buffer (get-buffer old-shell)
		   (rename-buffer "*shell*"))))))))

(defun ix-comint-clear-buffer ()
  (interactive)
  (save-excursion
    (ix-comint-bol)
    (kill-region (point-min) (point))))

(defun ix-comint-bol ()
  (interactive)
  (while (not (looking-at "^"))
    (backward-char 1)))

(global-set-key "\C-cs" 'ix-shell)

;(setq shell-mode-hook nil)
(add-hook 'shell-mode-hook
	  (function (lambda ()
		      (let ((map (current-local-map)))
			(setq comint-prompt-regexp "^[^#$%>]*[#$%>] *\\|^(dbx) \\|^(gdb) ")
			(define-key map "\C-c\C-a" 'ix-comint-bol)
			(define-key map "\e=" 'comint-dynamic-list-filename-completions)
			(define-key map "\C-cl" 'ix-comint-clear-buffer)
			(setq comint-input-ring-file-name (expand-file-name "~/.custom/sh-history"))
			(setq comint-input-ring-size 100)
			(make-local-variable 'font-lock-defaults)
			(setq font-lock-defaults '(shell-font-lock-keywords t t))
			))))

(defface font-lock-shell-prompt
  `((t (:foreground "brown" :bold t))) "" :group 'mllog)
(defface font-lock-shell-input
  `((t (:foreground "brown" :bold nil))) "" :group 'mllog)
(setq shell-font-lock-keywords
      (append '(("^\\(/[^\\$]+\\$ \\)[ ]*\\(.*\\)$"
		(1 font-lock-shell-prompt)
		(2 font-lock-shell-input)))))

;;;
;;; Font lock customizations.
;;;
(cond ((eq ix-emacs-type 'xemacs)
       ;(setq font-lock-auto-fontify t) ; doesn't seem to work
       (add-hook 'c++-mode-hook 'turn-on-font-lock)
       (add-hook 'java-mode-hook 'turn-on-font-lock)
       (add-hook 'makefile-mode-hook 'turn-on-font-lock)
       (add-hook 'perl-mode-hook 'turn-on-font-lock)
       (add-hook 'shell-mode-hook 'turn-on-font-lock)
       (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
       (add-hook 'gdb-mode-hook 'turn-on-font-lock)
       (add-hook 'outline-mode-hook 'turn-on-font-lock)
       )
      ((eq ix-emacs-type 'gnuemacs)
       (global-font-lock-mode 1)
       ))
