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

;; (setq lsp-print-io t)

;; (find-file "~/repos/md/massdrop-1/packages/basecamp/src/components/pages/DropConfig/Description.tsx")

(run-test "javascript-typescript")
