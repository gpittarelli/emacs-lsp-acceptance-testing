;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/go/Helen/main.go")))

(run-test "go")
