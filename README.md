# Completion at Point Function for Q-mode

A setup for getting documentation and auto completion for
[`q-mode`](https://github.com/psaris/q-mode).
This is *not an LSP* and only provides for auto completion and `eldoc`.

The goal is to make use of builtin q features to
extract then cache variable names, types, function parameters,
table columns and dictionary keys from a live session.

Features:
- Autocompletion for functions and variables with a completion at point function
- Quick Documentation through `eldoc`
- Scraping live session functions and variables
- Documentation for builtin functions and variables

# Installation and Config Setup

A sample configuration with [`q-mode`](https://github.com/psaris/q-mode) and [`corfu`](https://github.com/minad/corfu) frontend:

``` emacs-lisp
(use-package corfu
  :init
  (global-corfu-mode))

(use-package 'q-mode
  :defer t
  ;; add hook to completion at point function
  :hook (q-mode . (lambda ()
                    (setq-local completion-at-point-functions #'q-capf-completion-at-point)))
  :config
  ;; load the package after q-mode is loaded
  (require 'cape-q))
```

To load user global variables, run the following function i.e. `M-x cape-q-refresh-cache` with an active q-buffer.
To load user variables of all namespaces, run the function with prefix i.e. `C-u M-x cape-q-refresh-cache` with an active q-buffer.

I recommend binding `cape-q-refresh-cache` to a key and customizing the input parameters.
