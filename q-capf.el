;;; q-capf.el --- Completion at point function for q-mode -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Justin Yu <jusytinyu@gmail.com>

;; Author: Justin Yu
;; Keywords: tools, languages, wp
;; Homepage: https://github.com/Gchouchou/q-capf
;; Created 1 Jan 2025
;; Version: 0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Provide a completion at point function for q-mode for builtin and user defined
;;; variables and functions. To scrape variables and functions from a live session
;;; and add it to the cache, use `q-capf-refresh-cache'.

;;; The package also uses the cache to provide documentation using eldoc.

;;; Requirements:
;;; Package-Requires: ((emacs "27.1") (q-mode))

;;; Code:

(require 'json)
(require 'q-mode)

(defcustom q-capf-excluded-namespaces ()
  "List of namespaces to exclude."
  :type '(repeat string)
  :group 'q-capf)

(defvar q-capf-session-vars (make-hash-table :size 5 :test 'equal)
  "Hashmap of namespaces:variable/function:documentation.")

(defconst q-capf-builtin-vars
  (eval-when-compile
    (let* ((dir (cond
                 (load-file-name)
                 (buffer-file-name)
                 ((boundp 'byte-compile-current-file) byte-compile-current-file)))
           (file (concat (file-name-directory dir)
                         "builtins.json")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (json-parse-buffer))))
  "Hash table with builtin functions, variables and namespaces.")

(defvar q-capf--temp-output ""
  "String variable to hold q process output. Used in `q-capf-json-output-filter'.")

(defvar q-capf-function
  (eval-when-compile
    (q-strip
     (let* ((dir (cond
                 (load-file-name)
                 (buffer-file-name)
                 ((boundp 'byte-compile-current-file) byte-compile-current-file)))
            (file (concat (file-name-directory dir)
                          "query_env.q")))
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string)))))
  "String containing the q lambda to scrape session variables.")

(defvar q-capf--namespace ""
  "Namespace string for `q-capf-completion-at-point'.")

(defconst q-capf-type-hashmap
  (eval-when-compile
    (let* ((table (make-hash-table :test 'eql :size 50)))
      (puthash 0 "list" table)
      (puthash -1 "boolean" table)
      (puthash -2 "guid" table)
      (puthash -4 "byte" table)
      (puthash -5 "short" table)
      (puthash -6 "int" table)
      (puthash -7 "long" table)
      (puthash -8 "real" table)
      (puthash -9 "float" table)
      (puthash -10 "char" table)
      (puthash -11 "symbol" table)
      (puthash -12 "timestamp" table)
      (puthash -13 "month" table)
      (puthash -14 "date" table)
      (puthash -15 "datetime" table)
      (puthash -16 "timespan" table)
      (puthash -17 "minute" table)
      (puthash -18 "second" table)
      (puthash -19 "time" table)
      (puthash 77 "anymap" table)
      (puthash 97 "nested sym enum" table)
      (puthash 98 "table" table)
      (puthash 99 "dictionary" table)
      (puthash 100 "lambda" table)
      (puthash 101 "unary primitive" table)
      (puthash 102 "operator" table)
      (puthash 103 "iterator" table)
      (puthash 104 "projection" table)
      (puthash 105 "composition" table)
      (puthash 106 "each modified function" table)
      (puthash 107 "over accumulator function" table)
      (puthash 108 "scan accumulator function" table)
      (puthash 109 "each parallel or each prior" table)
      (puthash 110 "each right" table)
      (puthash 111 "each left" table)
      (puthash 112 "dynamic load" table)
      table))
  "Hashmap that maps q-type integers to strings.")

(defun q-capf-describe-type (type)
  "Return descriptive string for q integer or string TYPE."
  (cond
   ((stringp type) type)
   ((<= 1 type 19) (concat "list of " (gethash (- type) q-capf-type-hashmap)))
   ((<= 78 type 96) (concat "mapped list of lists of type " (gethash (- type 77) q-capf-type-hashmap)))
   ((<= 20 type 76) "enums")
   (t (gethash type q-capf-type-hashmap))))

;;;###autoload
(defun q-capf-refresh-cache (&optional all_names session)
  "Scrapes variables and functions in global namespace from `q-active-buffer'.
Populates `q-capf-session-vars' cache for `q-capf-completion-at-point'.

If ALL_NAMES is not nil, scrape all other namespaces except those
in q-capf-excluded-namespaces.
If SESSION is not nil, scrape from SESSION instead, buffer or buffer string
or handle name."
  (interactive "P")
  ;; default to q-active-buffer
  (let* ((session (or session q-active-buffer))
         (param (format "[%s;%s]"
                        (if all_names 1 0)
                        (pcase (length q-capf-excluded-namespaces)
                          (0 "()")
                          (1 (format "enlist `%s" (car q-capf-excluded-namespaces)))
                          (_ (format "(%s)"
                                     (mapconcat (lambda (namespace)
                                                  (format "`%s" namespace))
                                                q-capf-excluded-namespaces
                                                ";"))))))
         (full-body (concat q-capf-function param ", \"\\n\"")))
    (cond
     ((not (or (buffer-live-p session) (stringp session))) (error "No session provided and no q-active-buffer provided"))
     ((and (get-buffer session) (not (comint-check-proc (get-buffer session)))
           (error "Found buffer %s but comint did not find any processes" (get-buffer session))))
     ;; it matches a buffer and is a q session
     ((and (get-buffer session)
           (string-match "\*q-.*\*"
                         (with-current-buffer (get-buffer session)
                           (buffer-name))))
      (when all_names
        ;; full reset cache
        (setq q-capf-session-vars (make-hash-table :size 5 :test 'equal))
        (message "Refreshing cache for all namespaces"))
      (setq q-capf--temp-output "")
      (with-current-buffer (get-buffer session)
        (add-hook 'comint-preoutput-filter-functions #'q-capf-json-output-filter 0 t)
        (save-excursion
          (goto-char (point-max))
          (insert "1 " full-body ";")
          (comint-send-input nil t))))
     (t (let* ((handle (if-let* ((buffer (get-buffer session))
                                 (name (with-current-buffer buffer
                                         (buffer-name)))
                                 ((string-match "\*qcon-\\(.*\\)\*" name)))
                           (match-string 1 name)
                         session))
               ;; first escape \ with \\
               (escaped-body (replace-regexp-in-string
                              "\\\\" "\\\\"
                              full-body nil t))
               ;; escape apostrophe " with \"
               (escaped-body (replace-regexp-in-string
                              "\"" "\\\""
                              escaped-body nil t))
               (file (make-temp-file "q-scrape-" nil ".q"
                                     (format "1 (`$\":%s\") \"%s\";"
                                             handle
                                             escaped-body)))
               (table (condition-case err
                          (with-temp-buffer
                            (call-process q-program nil (current-buffer) nil file "-q")
                            (goto-char (point-min))
                            (json-parse-buffer))
                        (t (message (error-message-string err)) nil))))
          (when (hash-table-p table)
            (when all_names
              ;; full reset cache
              (setq q-capf-session-vars (make-hash-table :size 5 :test 'equal))
              (message "Refreshing cache for all namespaces"))
            (mapc (lambda (name)
                    (puthash name (gethash name table) q-capf-session-vars))
                  (hash-table-keys table))
            (message "Successful refresh of cache"))
          (delete-file file))))))

(defun q-capf-json-output-filter (output)
  "A oneshot filter that process OUTPUT from q or qcon process.
It should be added to `comint-preoutput-filter-functions' and will remove
itself after processing a new line input.

It stores the temporary string in `q-capf--temp-output' and then puts
the hashmap in `q-capf-session-vars'."
  (let* ((nline-index (string-match "\n" output)))
    (setq q-capf--temp-output (concat q-capf--temp-output (substring output 0 nline-index)))
    (if-let* ((nline-index)
              (table (condition-case nil
                         (json-parse-string (replace-regexp-in-string comint-prompt-regexp "" q-capf--temp-output))
                       ;; we should get a hashtable, instead give t so we pass if-let
                       (t (message "Not a json, input string was %s" q-capf--temp-output) t))))
        (prog1
            (if (hash-table-p table)
                (progn
                  ;; push every value to key at q-capf-session-vars
                  (mapc (lambda (name)
                          (puthash name (gethash name table) q-capf-session-vars))
                        (hash-table-keys table))
                  (message "Sucessful refresh of cache")
                  (substring output nline-index))
              (concat q-capf--temp-output (substring output nline-index)))
          (or (remove-hook 'comint-preoutput-filter-functions #'q-capf-json-output-filter t)
              (remove-hook 'comint-preoutput-filter-functions #'q-capf-json-output-filter))
          (setq q-capf--temp-output ""))
      "")))

(defun q-capf-get-doc (symbol &optional namespace)
  "Get documentation for SYMBOL in NAMESPACE.
If NAMESPACE is nil or empty string, assume default or global namespace.

Searches documentation in `q-capf-session-vars' and `q-capf-builtin-vars'."
  (interactive "sSymbol: \nsNamespace:")
  (if (and (stringp namespace) (> (length namespace) 0))
      (when-let ((namespace-hashtable (or (gethash namespace q-capf-session-vars)
                                          (gethash namespace q-capf-builtin-vars))))
        (gethash symbol namespace-hashtable))
    (or (gethash symbol (gethash "" q-capf-builtin-vars))
        (gethash symbol (gethash "q" q-capf-builtin-vars))
        (when-let ((session-vars (gethash "" q-capf-session-vars)))
          (gethash symbol session-vars)))))

;;;###autoload
(defun q-capf-completion-at-point ()
  "Completion at point function for q-mode.

Auto completes variables and functions with candidates from
`q-capf-session-vars' and `q-capf-builtin-vars'."
  (when (and (hash-table-p q-capf-session-vars)
             ;; do not trigger inside comments and strings
             (not (nth 3 (syntax-ppss)))
             (not (nth 4 (syntax-ppss))))
    (let* ((bounds (q-capf--bounds))
           (begin (car bounds))
           (end (cdr bounds))
           ;; split into namespace and variable
           (var (buffer-substring-no-properties begin end))
           (namespace (when (string-match
                             "\\.\\([a-zA-Z][a-zA-Z0-9_]*\\(\\.[a-zA-Z0-9_]+\\)*\\)\\.[a-zA-Z0-9_]*"
                             var)
                        (match-string 1 var)))
           (scandidates (if-let* ((docs (and namespace
                                             (or (gethash namespace q-capf-session-vars)
                                                 (gethash namespace q-capf-builtin-vars)))))
                            (progn (setq begin (+ begin 2 (length namespace)))
                                   (hash-table-keys docs))
                          ;; default namespace
                          (setq namespace nil)
                          (append (when-let* ((session-vars (gethash "" q-capf-session-vars)))
                                    (hash-table-keys session-vars))
                                  (hash-table-keys (gethash "" q-capf-builtin-vars))
                                  (hash-table-keys (gethash "q" q-capf-builtin-vars))
                                  ;; add namespaces to global namespace
                                  (mapcar (lambda (name)
                                            (format ".%s." name))
                                          ;; remove empty string namespace
                                          (delete "" (append (hash-table-keys q-capf-session-vars)
                                                             (hash-table-keys q-capf-builtin-vars))))))))
      ;; make sure we give more than one candidate
      (unless (zerop (length scandidates))
        (setq q-capf--namespace namespace)
        (list begin
              end
              scandidates
              :exclusive 'no
              :annotation-function
              (lambda (cand)
                (format " %s"
                        (if-let* ((doc (q-capf-get-doc cand q-capf--namespace))
                                  (type (gethash "type" doc)))
                            (q-capf-describe-type type)
                          (if (string-match-p "^\\..*\\.$" cand)
                              "namespace"
                            "any"))))
              :company-doc-buffer
              (lambda (cand)
                (when-let* ((doc (q-capf-get-doc cand q-capf--namespace))
                            (docs (hash-table-keys doc))
                            (body (string-join
                                   (delete
                                    nil
                                    (list
                                     (when (member "type" docs)
                                       (format "%s is a %s." cand (q-capf-describe-type (gethash "type" doc))))
                                     (when (member "doc" docs)
                                       (format "%s" (gethash "doc" doc)))
                                     (when (member "cols" docs)
                                       ;; cols is converted to a vector
                                       (format "Table Columns:\n%s"
                                               (string-join (gethash "cols" doc) ", ")))
                                     (when (member "keys" docs)
                                       ;; keys is converted to a vector
                                       (format "Dictionary Keys:\n%s"
                                               (string-join (gethash "keys" doc) ", ")))
                                     (when (member "param" docs)
                                       ;; params is converted to a vector
                                       (if (< 0 (length (gethash "param" doc)))
                                           (format "Function Parameters Names:\n%s"
                                                   (string-join (gethash "param" doc) ", "))
                                         "Function takes in no parameters"))
                                     (when (member "file" docs)
                                       (concat (format "Function source file: %s" (gethash "file" doc))
                                               (when (member "line" docs) (format "\nline:%s" (gethash "line" doc)))))
                                     (when (member "body" docs)
                                       (format "Function Body:\n%s" (gethash "body" doc)))))
                                   "\n")))
                  (with-current-buffer (get-buffer-create "*documentation*")
                    (erase-buffer)
                    (fundamental-mode)
                    (save-excursion
                      (insert body)
                      (visual-line-mode))
                    (current-buffer)))))))))

;;; eldoc functions

(defun q-capf--bounds ()
  "Return the bounds of a variable or function in q.
If it cannot match a valid variable it will give begin and end bounds at point."
  (save-excursion
    (let* ((initial (point))
           (begin (prog2 (while (and (not (bobp))
                                     ;; skip any word chars or period or underscore
                                     (progn (skip-syntax-backward "w")
                                            (or (eq (char-before) ?.)
                                                (eq (char-before) ?_))))
                           (backward-char))
                      (point)))
           ;; try to match the string
           (end (if (and (re-search-forward "[a-zA-Z0-9_.]*" nil 'move 1)
                         ;; confirm it starts at begin
                         (eq (match-beginning 0) begin))
                    (point)))
           (begin (if (not end) initial begin))
           (end (or end initial)))
      (cons begin end))))

(defun q-capf-eldoc-get-bounds ()
  "Function used by `q-capf-eldoc' to get the bounds of q variable/function.
Returns ((start . end) . index), where index is used for function parameters."
  (save-excursion
    ;; move backward when there is a [
    (when (eq (char-after) 91) (backward-char))
    ;; TODO: index is not used, could make a simple checker
    (cons (q-capf--bounds) -1)))

;;;###autoload
(defun q-capf-eldoc (callback &rest _ignored)
  "Print q var documentation by calling CALLBACK.

Searches for the var at point through the hashtables `q-capf-builtin-vars'
and `q-capf-session-vars'."
  (when-let* ((meta (when (and q-capf-session-vars
                               (hash-table-p q-capf-session-vars)
                               ;; do not trigger inside comments and strings
                               (not (nth 3 (syntax-ppss)))
                               (not (nth 4 (syntax-ppss))))
                      (q-capf-eldoc-get-bounds)))
              (bounds (car meta))
              (param-index (cdr meta))
              (thing (buffer-substring-no-properties (car bounds) (cdr bounds)))
              (face 'font-lock-variable-name-face)
              (doc (if (string-match
                        "\\.\\([a-zA-Z][a-zA-Z0-9_]*\\(\\.[a-zA-Z0-9_]+\\)*\\)\\.\\([a-zA-Z0-9_]+\\)"
                        thing)
                       (when-let* ((namespace (match-string 1 thing))
                                   (var (substring thing (+ 2 (length namespace))))
                                   (docs (or (gethash namespace q-capf-session-vars)
                                             (when-let* ((x (gethash namespace q-capf-builtin-vars)))
                                               (pcase namespace
                                                 ("z" (setq face 'font-lock-constant-face))
                                                 (_ (setq face 'font-lock-builtin-face)))
                                               x))))
                         (gethash var docs))
                     (or (when-let* ((x (gethash thing (gethash "" q-capf-builtin-vars))))
                           (setq face 'font-lock-keyword-face) x)
                         (when-let* ((x (gethash thing (gethash "q" q-capf-builtin-vars))))
                           (setq face 'font-lock-builtin-face) x)
                         (when (gethash "" q-capf-session-vars)
                           (gethash thing (gethash "" q-capf-session-vars))))))
              (entries (hash-table-keys doc))
              (type-string (if-let* ((type (gethash "type" doc)))
                               (q-capf-describe-type type)
                             "any"))
              ;; first use function params
              (docstr (cond ((member "param" entries)
                             (let* ((params (mapcar (lambda (s) (upcase (copy-sequence s))) (gethash "param" doc)))
                                    (param (when (>= param-index 0) (nth param-index params))))
                               (when param (add-text-properties 0 (length param) '(face bold) (nth param-index params)))
                               (format "%s: param: %s"
                                       type-string
                                       (if (> (length params) 0)
                                           (string-join params " ")
                                         "()"))))
                            ;; then give table columns
                            ((member "cols" entries)
                             (format "%s: cols: (%s)"
                                     type-string
                                     (string-join (let ((cols (gethash "cols" doc)))
                                                    (cl-subseq cols 0 (min (length cols) 20)))
                                                "; ")))
                            ;; dictionary keys
                            ((member "keys" entries)
                             (format "%s: keys: (%s)"
                                     type-string
                                     (string-join (let ((keys (gethash "keys" doc)))
                                                    (cl-subseq keys 0 (min (length keys) 20)))
                                                "; ")))
                            ((member "doc" entries)
                             (format "%s: doc: %s"
                                     type-string
                                     (gethash "doc" doc)))
                            ((member "body" entries)
                             (format "%s: body: %s"
                                     type-string
                                     (gethash "body" doc)))
                            (t (format "%s"
                                       type-string)))))
    (put-text-property 0 (length type-string) 'face 'font-lock-type-face docstr)
    (funcall
     callback
     docstr
     :thing thing
     :face face)))

(provide 'q-capf)
;;; q-capf.el ends here
