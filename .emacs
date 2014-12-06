(if (eq system-type 'windows-nt)
    ()	; (setq load-path (cons "c:\\..." load-path))
  ()	; (setq load-path (cons "/cygdrive/c/..." load-path))
)

(setq ix-power-user-p t)

(load "ix-init")
(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)
(if (eq system-type 'windows-nt)
    (setq shell-file-name "C:\\cygwin\\Cygwin.bat"))

(require 'ispell)
(if (setq ispell-program-name (executable-find "aspell"))
    ()
  (let ((cygwin-path "c:/cygwin/bin"))
    (if (file-exists-p cygwin-path)
	(setq exec-path (cons cygwin-path exec-path)))
    (setq ispell-program-name (executable-find "aspell"))))
