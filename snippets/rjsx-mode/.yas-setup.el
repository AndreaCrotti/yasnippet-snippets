;;; .yas-setup.el --- Yasnippet helper functions for JSX snippets

;;; Commentary:

;;; Code:

(require 'yasnippet)

(defun yas-jsx-get-class-name-by-file-name ()
  "Return name of class-like construct by `file-name'."
  (if buffer-file-name
      (let ((class-name (file-name-nondirectory
                         (file-name-sans-extension buffer-file-name))))
        (if (equal class-name "index")
            (file-name-nondirectory
             (directory-file-name (file-name-directory buffer-file-name)))
          class-name))
    ""))

(defun yas-snake-case (s)
  "Convert S to snake-case."
  (mapconcat #'upcase (split-string s "[^[:word:]0-9]+" t) "_"))

;;; .yas-setup.el ends here
