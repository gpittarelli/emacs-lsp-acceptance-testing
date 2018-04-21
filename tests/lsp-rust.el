;;; lsp-rust.el --- Testing rust LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/rust/futures/futures-async-runtime/src/stream.rs")))

(run-test "rust")
