;;; -*- lexical-binding: t -*-
(require 'yasnippet)
(declare-function yasnippet-snippets--no-indent "yasnippet-snippets")

(add-hook 'bibtex-mode-hook #'yasnippet-snippets--no-indent)
