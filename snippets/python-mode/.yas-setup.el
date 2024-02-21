;;; -*- lexical-binding: t -*-
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
