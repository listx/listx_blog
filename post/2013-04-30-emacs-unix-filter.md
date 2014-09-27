---
title: Emacs: Using a Unix Filter
tags: emacs
---

In Vim, you can easily call an external text utility on a region of text, then replace that selected region with the output of the text utility.
This is very useful for doing bulk transformations quickly.

I coded up a very stupid commenting program called [`nox`][nox] that comments/uncomments text.
Here is how you'd use it straight from the terminal:

```{.bash .numberLines}
$ echo "hi" | nox -l c
//hi
```

By default nox comments text, and the `-l c` option sets it to C-styled comments, `//`.

## Emacs

I use [evil-mode][evil-mode] with Emacs.
Here is my relevant `.emacs` to set up `nox` correctly:

- i emacs-unix-filter.el

This is certainly not the most beautiful code, but it works.

### References
- <http://stackoverflow.com/questions/206806/filtering-text-through-a-shell-command-in-emacs>
- <http://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region>

[nox]:https://github.com/listx/nox
[evil-mode]:http://gitorious.org/evil/pages/Home
