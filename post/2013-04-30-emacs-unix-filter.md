---
title: Emacs: Using a Unix Filter
tags: emacs
---

In Vim, you can easily call an external text utility on a region of text, then replace that selected region with the output of the text utility.
This is very useful for doing bulk transformations quickly.

I coded up a very stupid commenting program called [`nox`][nox] that comments/uncomments text.
Here is how you'd use it straight from the terminal:

```
$ echo "hi" | nox -l c
//hi

```

By default nox comments text, and the `-l c` option sets it to C-styled comments, `//`.

## Emacs

I use [evil-mode][evil-mode] with Emacs.
Here is my relevant `.emacs` to set up `nox` correctly:

```commonlisp
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
    (my-addrem-comment-region (line-beginning-position) (line-beginning-position 2) f)
  ))
(define-key evil-visual-state-map ",c" (lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-visual-state-map ",C" (lambda () (interactive) (my-addrem-comment nil))) ; remove comment
(define-key evil-normal-state-map ",c" (lambda () (interactive) (my-addrem-comment t))) ; add comment
(define-key evil-normal-state-map ",C" (lambda () (interactive) (my-addrem-comment nil))) ; remove comment
```

This is certainly not the most beautiful code, but it works.

### References
- <http://stackoverflow.com/questions/206806/filtering-text-through-a-shell-command-in-emacs>
- <http://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region>

[nox]:https://github.com/listx/nox
[evil-mode]:http://gitorious.org/evil/pages/Home
