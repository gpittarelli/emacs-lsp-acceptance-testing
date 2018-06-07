;;; lsp-go.el --- Testing javascript Go mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/go/Helen/main.go")))

(run-test "go")
