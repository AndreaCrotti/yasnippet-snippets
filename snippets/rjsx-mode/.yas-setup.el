;;; .yas-setup.el --- Yasnippet helper functions for JSX snippets

;;; Commentary:

;;; Code:

(require 'yasnippet)
(require 's)

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

;;; .yas-setup.el ends here
