;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(defun my-js-mode-setup ()
  (interactive)
  (company-mode)
  (eldoc-mode t)
  (flycheck-mode)
  (lsp-javascript-typescript-enable)
  (unless noninteractive
    ;;   (setq-local eldoc-message-function #'my-eldoc-display-message)
    (lsp-ui-mode)))

(find-file (file-truename (concat repodir "/tests/fixtures/js/c.ts")))

(run-test "javascript-typescript")
