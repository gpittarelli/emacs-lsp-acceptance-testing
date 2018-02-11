;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/rust/futures/src/lock.rs")))

(run-test "rust")
