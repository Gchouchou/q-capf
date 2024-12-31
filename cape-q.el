;;; Code:

(require 'json)
(require 'q-mode)
(require 'cape)

(defcustom cape-q-excluded-namespaces ()
  "List of namespaces to exclude."
  :type 'list
  :group 'cape-q)

(defvar cape-q-session-vars (make-hash-table :size 5 :test 'equal)
  "Hashmap of namespaces:variable/function:documentation.")

(defvar cape-q--temp-output ""
  "String variable to hold q process output. Used in `cape-q-json-output-filter'.")

(defconst cape-q-base (file-name-directory load-file-name)
  "The path to the package.")

(defvar cape-q-function
  (q-strip
   (with-temp-buffer
     (insert-file-contents (concat (file-name-directory load-file-name) "/" "query_env.q"))
     (buffer-string)))
  "String containing a q lambda.")

(defvar cape-q--namespace ""
  "Namespace string for capf.")

(defconst cape-q-type-hashmap
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
    table)
  "Hashmap that maps q-type integers to strings.")

(defun cape-q-describe-type (type)
  "Return descriptive string for q integer TYPE."
  (cond
   ((<= 1 type 19) (concat "list of " (gethash (- type) cape-q-type-hashmap)))
   ((<= 78 type 96) (concat "mapped list of lists of type " (gethash (- type 77) cape-q-type-hashmap)))
   ((<= 20 type 76) "enums")
   (t (gethash type cape-q-type-hashmap))))

(defun cape-q-refresh-cache (&optional only_global session)
  "Scrapes variables and functions in global namespace from active session.
Then refresh the cache of the completion at point function.

If ONLY_GLOBAL is not nil, scrape all other namespaces except those
in cape-q-excluded-namespaces.
If SESSION is not nil, scrape from SESSION instead, buffer or buffer string
or handle name."
  (interactive "P")
  ;; default to q-active-buffer
  (let* ((session (or session q-active-buffer)))
    (cond
     ((not (or (bufferp session) (stringp session))) (error "No session provided and no q-active-buffer provided"))
     ((and (get-buffer session) (not (comint-check-proc (get-buffer session)))
           (error "Found buffer %s but comint did not find any processes" (get-buffer session))))
     ;; it matches a buffer and is a q session
     ((and (get-buffer session)
           (string-match "\*q-.*\*"
                         (with-current-buffer (get-buffer session)
                           (buffer-name))))
      (unless only_global
        ;; full reset cache
        (setq cape-q-session-vars (make-hash-table :size 5 :test 'equal)))
      (setq cape-q--temp-output "")
      (with-current-buffer (get-buffer session)
        (add-hook 'comint-preoutput-filter-functions #'cape-q-json-output-filter 0 t)
        (save-excursion
          (goto-char (point-max))
          (insert "1 "
                  cape-q-function
                  (format "[%s;%s]"
                          (if only_global 1 0)
                          (pcase (length cape-q-excluded-namespaces)
                            (0 "()")
                            (1 (format "enlist `%s" (car cape-q-excluded-namespaces)))
                            (_ (format "(%s)"
                                       (mapconcat (lambda (namespace)
                                                    (format "`%s" namespace))
                                                  cape-q-excluded-namespaces
                                                  ";")))))
                  ", \"\\n\";")
          (comint-send-input nil t))))
     (t (let* ((handle (if-let* ((buffer (get-buffer session))
                                 (name (with-current-buffer buffer
                                         (buffer-name)))
                                 ((string-match "\*qcon-\\(.*\\)\*" name)))
                           (match-string 1 name)
                         session))
               ;; first escape \ with \\
               (body (replace-regexp-in-string
                      "\\\\" "\\\\"
                      cape-q-function nil t))
               ;; escape apostrophe " with \"
               (body (replace-regexp-in-string
                      "\"" "\\\""
                      body nil t))
               (file (make-temp-file "q-scrape-" nil ".q"
                                     (format "1 `$\":%s\" \"%s\";"
                                             handle
                                             body)))
               (table (with-temp-buffer
                         (call-process q-program nil (current-buffer) nil "-q" file)
                         (goto-char (point-min))
                         (json-parse-buffer))))
          (message "using handle: %s" handle)
          (when (hash-table-p table)
            (unless only_global
              ;; full reset cache
              (setq cape-q-session-vars (make-hash-table :size 5 :test 'equal)))
            (mapc (lambda (name)
                    (puthash name (gethash name table) cape-q-session-vars))
                  (hash-table-keys table)))
          (delete-file file))))))

(defun cape-q-json-output-filter (output)
  "A oneshot filter that process OUTPUT from q or qcon process.
It should be added to `comint-preoutput-filter-functions' and will remove
itself after processing a new line input.

It stores the temporary string in `cape-q--temp-output' and then puts
 the hashmap in `cape-q-session-vars'."
  (let* ((nline-index (string-match "\n" output)))
    (setq cape-q--temp-output (concat cape-q--temp-output (substring output 0 nline-index)))
    (if-let* ((nline-index)
              (table (condition-case nil
                          (json-parse-string cape-q--temp-output)
                        ;; we should get a hashtable, instead give t so we pass if-let
                        (t t))))
        (prog1
            (if (hash-table-p table)
                (progn
                  ;; push every value to key at cape-q-session-vars
                  (mapc (lambda (name)
                          (puthash name (gethash name table) cape-q-session-vars))
                        (hash-table-keys table))
                  (substring output nline-index))
              (concat cape-q--temp-output (substring output nline-index)))
          (or (remove-hook 'comint-preoutput-filter-functions #'cape-q-json-output-filter t)
              (remove-hook 'comint-preoutput-filter-functions #'cape-q-json-output-filter))
          (setq cape-q--temp-output ""))
      "")))

(defun cape-q-completion-at-point ()
  "Completion at point function for q-mode.

Auto completes variables and functions."
  (interactive)
  (when (hash-table-p cape-q-session-vars)
    (let* ((bounds (cape-q--bounds))
           (begin (car bounds))
           (end (cdr bounds))
           ;; split into namespace and variable
           (var (buffer-substring-no-properties begin end))
           (namespace (when (string-match
                             "\\.\\([a-zA-Z][a-zA-Z0-9_]*\\(\\.[a-zA-Z0-9_]+\\)*\\)\\.[a-zA-Z0-9_]*"
                             var)
                        (match-string 1 var)))
           (begin (if namespace (+ begin 2 (length namespace)) begin))
           (candidates (gethash (or namespace "") cape-q-session-vars))
           (scandidates (append (when candidates (hash-table-keys candidates))
                               ;; add namespaces to global namespace
                               (unless namespace
                                 (mapcar (lambda (name)
                                           (concat "." name))
                                         ;; remove first element with cdr
                                         (delete "" (hash-table-keys cape-q-session-vars)))))))
      (setq cape-q--namespace (or namespace ""))
      (list begin
            end
            scandidates
            :annotation-function (lambda (cand)
                                   (if-let* ((candidates (gethash cape-q--namespace cape-q-session-vars))
                                             (doc (gethash cand candidates))
                                             (type (gethash "type" doc)))
                                       (cape-q-describe-type type)
                                     ;; it has to be a namespace if missing
                                     "namespace"))
            :company-doc-buffer
            (lambda (cand)
              (when-let* ((candidates (gethash cape-q--namespace cape-q-session-vars))
                          (doc (gethash cand candidates))
                          (docs (hash-table-keys doc)))
                (with-current-buffer (get-buffer-create "*documentation*")
                  (erase-buffer)
                  (fundamental-mode)
                  (save-excursion
                    (when (member "type" docs)
                      (insert (format "%s is a %s.\n\n" cand (cape-q-describe-type (gethash "type" doc)))))
                    (when (member "cols" docs)
                      ;; cols is converted to a vector
                      (insert (format "Table Columns:\n%s\n\n"
                                      (mapconcat #'identity (gethash "cols" doc) ", "))))
                    (when (member "keys" docs)
                      ;; keys is converted to a vector
                      (insert (format "Dictionary Keys\n%s\n\n"
                                      (mapconcat #'identity (gethash "keys" doc) ", "))))
                    (when (member "param" docs)
                      ;; params is converted to a vector
                      (insert (format "Function Parameters Names:\n%s\n\n"
                                      (mapconcat #'identity (gethash "param" doc) ", "))))
                    (when (member "file" docs)
                      (insert (format "Function source file: %s\n\n" (gethash "file" doc))))
                    (when (member "body" docs)
                      (insert (format "Function Body:\n%s\n\n" (gethash "body" doc))))
                    (visual-line-mode))
                  (current-buffer))))))))

(defun cape-q--bounds ()
  "Return the bounds of a variable or function in q.
If it cannot match a valid variable it will give begin and end bounds at point."
  (interactive)
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

(provide 'cape-q)
;;; cape-q.el ends here
