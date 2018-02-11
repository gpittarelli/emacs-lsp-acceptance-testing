;;; lsp-css.el --- Testing CSS LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/js/Frontend/src/scss/pages/lobby-list/_lobby-list.scss")))

(run-test "css")
