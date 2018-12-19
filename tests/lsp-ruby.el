;;; lsp-ruby.el --- Testing ruby LSP mode -*- lexical-binding: t; -*-

;; (setq lsp-print-io t)
;; (setq lsp-print-io nil)

(setq lsp-hover-text-function 'lsp--text-document-signature-help)

(find-file (file-truename (concat repodir "/tests/fixtures/ruby/http/lib/http/client.rb")))

;;(run-test "ruby")
