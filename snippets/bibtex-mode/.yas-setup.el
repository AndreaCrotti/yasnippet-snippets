(require 'yasnippet)
(require 'yasnippet-snippets)

(add-hook 'bibtex-mode-hook #'yasnippet-snippets--no-indent)
