;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/plain.txt")))

(when noninteractive (lsp-show-message-test-enable))
