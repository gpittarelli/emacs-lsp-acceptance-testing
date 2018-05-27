;;; lsp-rust.el --- Testing rust LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/tests/fixtures/R/ggplot.r")))

(run-test "R")
