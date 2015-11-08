# snip-it [![Build Status](https://travis-ci.org/jacksonrayhamilton/snip-it.png?branch=master)](https://travis-ci.org/jacksonrayhamilton/snip-it)

Expand snippets.

## Usage

Bind `snip-it-expand` to a key of your choice:

```elisp
(global-set-key (kbd "<C-tab>") #'snip-it-expand)
```

Save snippets to a directory with the name of a major mode.  Snippets' names are
determined by their filenames.  For instance, an "add" snippet for Emacs Lisp
would be saved to "~/.emacs.d/snippets/emacs-lisp-mode/add" and it would look
like this:

```elisp
(setq $1 (+ $1 $2))
```

Add the major mode's directory's parent to `snip-it-directories':

```elisp
(add-to-list 'snip-it-directories "~/.emacs.d/snippets")
```

In a lisp buffer, type "add" and invoke `snip-it-expand`.  Start typing.  Use
the same key bound to `snip-it-expand` to advance to the N+1th fields.

## Disclaimer

This package is not yet stable.  There are plenty of improvements and features
that may be added before this package sees an official release.
