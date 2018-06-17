(require 'yasnippet)
(defvar yas-text)

(defvar python-split-arg-arg-regex
"\\([[:alnum:]*]+\\)\\(:[[:blank:]]*[[:alpha:]]*\\)?\\([[:blank:]]*=[[:blank:]]*[[:alnum:]]*\\)?"
"Regular expression matching an argument of a python function.
First group should give the argument name.")

(defvar python-split-arg-separator
"[[:space:]]*,[[:space:]]*"
"Regular expression matching the separator in a list of argument.")

(defun python-split-args (arg-string)
  "Split a python argument string ARG-STRING into a tuple of argument names."
  (mapcar (lambda (x)
            (when (string-match python-split-arg-arg-regex x)
              (match-string-no-properties 1 x)))
          (split-string arg-string python-split-arg-separator t)))

(defun python-args-to-docstring ()
  "Return docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                          (lambda (x)
                            (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                                    (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                          args
                          indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))

(defun yas-snips-snippet-current-method-and-args ()
  "Return information on the current definition."
  (let ((current-defun (python-info-current-defun))
        (current-arglist
         (save-excursion
           (python-nav-beginning-of-defun)
           (when (re-search-forward "(" nil t)
             (let* ((start (point))
                    (end (progn
                           (forward-char -1)
                           (forward-sexp)
                           (- (point) 1))))
               (yas-snips-snippet-split-args
                (buffer-substring-no-properties start end))))))
        class method args)
    (when (not current-arglist)
      (setq current-arglist '("self")))
    (if (and current-defun
             (string-match "^\\(.*\\)\\.\\(.*\\)$" current-defun))
        (setq class (match-string 1 current-defun)
              method (match-string 2 current-defun))
      (setq class "Class"
            method "method"))
    (list class method current-arglist)))

(defun yas-snips-snippet-init-assignments (arg-string)
  "Return the typical __init__ assignments for arguments in ARG-STRING."
  (let ((indentation (make-string (save-excursion
                                    (goto-char start-point)
                                    (current-indentation))
                                  ?\s)))
    (mapconcat (lambda (arg)
                 (if (string-match "^\\*" arg)
                     ""
                   (format "self.%s = %s\n%s" arg arg indentation)))
               (yas-snips-snippet-split-args arg-string)
               "")))

(defun yas-snips-snippet-super-form ()
  "Return (Class, first-arg).method if Py2.
Else return ().method for Py3."
  (let* ((defun-info (yas-snips-snippet-current-method-and-args))
         (class (nth 0 defun-info))
         (method (nth 1 defun-info))
         (args (nth 2 defun-info))
         (first-arg (nth 0 args))
         (py-version-command " -c 'import sys ; print(sys.version_info.major)'")
         ;; Check for distribution-defined or user-defined python-interpreter.
         ;; If python-interpreter is not set, then py-version-command
         ;; will always return "2" for "python" on PEP 394-compliant
         ;; systems.
         (if (fboundp 'python-interpreter)
             (py-version-num (substring (shell-command-to-string (concat python-interpreter py-version-command)) 0 1))
           ;; Otherwise get the version from "python" in $PATH
           ;; This preserves support for virtualenvs that may have
           ;; either version 2 or 3 installed as "python"--and also
           ;; for distributions that install Python 3 to $PATH/python
           (py-version-num (substring (shell-command-to-string (concat "python" py-version-command)) 0 1))))
    (if (string-match py-version-num "2")
        (format "(%s, %s).%s" class first-arg method)
      (format "().%s" method))))

(defun yas-snips-snippet-super-arguments ()
  "Return the argument list for the current method."
  (mapconcat (lambda (x) x)
             (cdr (nth 2 (yas-snips-snippet-current-method-and-args)))
             ", "))
