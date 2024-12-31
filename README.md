# Auto completion for Q-mode

A setup for getting documentation and auto completion for
`q-mode`.
This is *not an LSP* and is only used for auto completion.

The goal is to make use of builtin q features to
extract then cache variable names, types, function parameters,
table columns and dictionary keys from a live session.

Features:
- Autocompletion of functions and variables with completion at point function
- Documentation and autocompletion for builtin keywords
- Caching of session functions and variables for autocompletion

# Installation and Config Setup

A sample configuration with `q-mode`:
``` emacs-lisp
(use-package 'q-mode
  :defer t
  ;; add hook to completion at point function
  :hook (q-mode . (lambda ()
                    (setq-local completion-at-point-functions #'cape-q-completion-at-point)))
  :config
  ;; load the package after q-mode is loaded
  (require 'cape-q))
```

To load user global variables, run the following function i.e. `M-x cape-q-refresh-cache` with an active q-buffer.
To load user variables of all namespaces, run the function with prefix i.e. `C-u M-x cape-q-refresh-cache` with an active q-buffer.

