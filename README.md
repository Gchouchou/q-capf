# Completion at Point Function for Q-mode

A setup for getting documentation and auto completion for
[`q-mode`](https://github.com/psaris/q-mode).
This is *not an LSP* and only provides a completion at point function and `eldoc`.

The goal is to make use of builtin q features to
extract then cache variable names, types, function parameters,
table columns and dictionary keys from a live session.

Features:
- Autocompletion for functions and variables with a completion at point function
- Quick Documentation through `eldoc`
- Scraping live session functions and variables
- Documentation for builtin functions and variables

# Installation and Config Setup

`q-capf` uses json to communicate q variables and functions, for that reason emacs needs to be compiled with json.
jansson compilation is not needed after emacs 30.1.

To enable auto completion, turn on the `q-capf-mode` minor mode. To turn on the
eldoc function, turn on `q-capf-eldoc-mode` minor mode. There are functions
`q-capf-mode-enable` and `q-capf-eldoc-mode-enable` to easily activate and load the package.

A sample configuration with [`q-mode`](https://github.com/psaris/q-mode) and [`corfu`](https://github.com/minad/corfu) frontend:

``` emacs-lisp
(use-package corfu
  :init
  (global-corfu-mode))

(use-package 'q-mode
  :defer t
  ;; add hook to activate `q-capf-mode'
  :hook (q-mode . q-capf-mode-enable))
```

To load user global variables, run the following function i.e. `M-x q-capf-refresh-cache` with an active q-buffer.
To load user variables of all namespaces, run the function with prefix i.e. `C-u M-x q-capf-refresh-cache` with an active q-buffer.

I recommend binding `q-capf-refresh-cache` to a key and customizing the input parameters.

# Files

There are two files that are necessary for the package to function, `builtins.json`, for the builtin function documentation, and `query_env.q`,
for the scraping function.

`builtins.json` contains documentation and parameters for builtin functions in the `q`, `h`, `j`, and default namesspaces.
The documentation comes from https://code.kx.com/q/ref/ but is not consistent and should be revised.
To extend the number of builtin variables, see `builtins.json` for the format required.

`query_env.q` contains the `q` lambda used to scrape from the live session. It returns a json string of a similar format
to `builtins.json`. Since namespaces are simply dictionaries, there is no difference between a dictionary and a namespace inside a dictionary.
Beacuse of this issue, `query_env.q` does not scrape deeper than one layer inside the namespace. There is no distinction between a sub namespace and
a dictionary.
