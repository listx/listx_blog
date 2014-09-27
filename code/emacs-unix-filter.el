(defun my-addrem-comment-region (b e f)
	"Use the `nox' command to comment the current region."
	(interactive)
	(shell-command-on-region
	; beginning and end of buffer
	b e
	; command and parameters
	(concat
		(if f
			"~/prog/nox/src/nox -l "
			"~/prog/nox/src/nox -u -l ")
			(case (with-current-buffer (current-buffer) major-mode)
				('c-mode "c")
				('emacs-lisp-mode "emacslisp")
				('haskell-mode "haskell")
				('LilyPond-mode "tex")
				('plain-tex-mode "tex")
				(t "shell")); default to shell
		)
	; output buffer
	(current-buffer)
	; replace?
	t
	; name of the error buffer
	"*nox Error Buffer*"
	; show error buffer?
	t
	)
)
(defun my-addrem-comment (f)
	(if (use-region-p)
		(progn
			(my-addrem-comment-region (region-beginning) (region-end) f)
			(evil-visual-char)
			(evil-exit-visual-state)
		)
		(my-addrem-comment-region
			(line-beginning-position) (line-beginning-position 2) f)
	)
)
(define-key evil-visual-state-map ",c"
	(lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-visual-state-map ",C" (lambda () (interactive)
	(my-addrem-comment nil))) ; remove comment
(define-key evil-normal-state-map ",c"
	(lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-normal-state-map ",C"
	(lambda () (interactive) (my-addrem-comment nil))) ; remove comment
