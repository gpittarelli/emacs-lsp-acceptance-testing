;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(defun my-js-mode-setup ()
  (company-mode)
  (lsp-typescript-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
;;    (setq-local eldoc-message-function #'my-eldoc-display-message)
    (lsp-ui-mode)))

(find-file (file-truename (concat repodir "/tests/fixtures/js/d.tsx")))

(run-test "typescript")
