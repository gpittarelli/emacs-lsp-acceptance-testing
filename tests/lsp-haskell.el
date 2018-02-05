;;; lsp-haskell.el --- Testing haskell LSP mode -*- lexical-binding: t; -*-

(find-file (file-truename (concat repodir "/servers/haskell-ide-engine/src/Haskell/Ide/Engine/Dispatcher.hs")))

(run-test "haskell")
