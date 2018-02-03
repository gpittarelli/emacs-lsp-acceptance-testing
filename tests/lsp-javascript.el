;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(toggle-debug-on-error)

(when noninteractive
  ;; Skip the first call of kill-emacs in batch mode -- it's emacs
  ;; trying to kill us cause it thinks we're done.
  (setq orig-kill-emacs (symbol-function 'kill-emacs))
  (defun kill-emacs (&rest yay)
    (fset 'kill-emacs orig-kill-emacs)))

(setq repodir (expand-file-name ".." (file-name-directory
                                      (or load-file-name (buffer-file-name)))))

(push (file-truename (concat repodir "servers/javascript-typescript-langserver-npm/node_modules/.bin/")) exec-path)
(push (file-truename (concat repodir "servers/typescript-language-server-npm/node_modules/.bin/")) exec-path)

(let ((default-directory  (expand-file-name "emacs/" repodir)))
  (normal-top-level-add-subdirs-to-load-path))

(require 'deferred)
(require 'lsp-mode)
(require 'lsp-javascript-typescript)
(require 'company)
(require 'pos-tip)
(require 's)
(require 'dash)
(require 'company-quickhelp)
(require 'company-lsp)
(require 'flycheck)
(require 'lsp-flycheck)

(push 'company-lsp company-backends)

(defvar my-show-pos-tip t)

;; autotesting in --batch: easy hooking into eldoc-message
(when noninteractive
  (setq abc nil)
  (defun eldoc-message (&rest args)
    (setq abc args))
  (defun eldoc-display-message-p ()
    t))

(defun my-eldoc-display-message (format-string &rest args)
  "Display eldoc message near point."
  (let ((m (if args
	       (apply 'format format-string args)
	     "")))
    (let ((message-log-max 10000))
      (when (and my-show-pos-tip
		 (> (length m) 0))
	(pos-tip-show m nil nil nil -1))
      (when (and (not my-show-pos-tip)
		 (not format-string)
		 (not args))
	(pos-tip-hide)
	))))

(defun my-js-mode-setup ()
  (company-mode)
  (lsp-javascript-typescript-enable)
  (eldoc-mode t)
  (flycheck-mode)
  ;; (unless noninteractive
  ;;   (setq-local eldoc-message-function #'my-eldoc-display-message))
  )

(defun my-hide-pos-tip ()
  (setq my-show-pos-tip nil)
  (pos-tip-hide))

(defun my-unhide-pos-tip ()
  (setq my-show-pos-tip t))

(add-hook 'focus-out-hook #'my-hide-pos-tip)
(add-hook 'focus-in-hook #'my-unhide-pos-tip)
(add-hook 'js-mode-hook #'my-js-mode-setup)

(add-to-list 'auto-mode-alist '("\\.jsx" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts" . js-mode))

(require 'js)
(define-key js-mode-map (kbd "M-.") #'xref-find-definitions)

;; Debugging:
;;(setq lsp-print-io t)
;;(setq lsp-print-io nil)

(find-file (file-truename (concat repodir "/tests/fixtures/js/a.js")))
(forward-line)

;; (profiler-start 'cpu+mem)

(defun advance ()
  (deferred:next
    (lambda ()
      (if (eobp)
          nil
        (forward-char)
        (funcall eldoc-documentation-function)
;;        (lsp--update-code-lenses)
        (step-hover 0)))))

(defun step-hover (i)
  (deferred:next
    (lambda ()
      (if (and (> i 10) (or abc (> i 100)))
          (progn
            (when abc
              (message "found at %s (%s): %s" i (symbol-at-point) abc))
            (when lsp-code-actions
              (message "found code actions for %s: %s" i (symbol-at-point) lsp-code-actions))

            (setq abc nil)
            (advance))
        (deferred:nextc
          (deferred:wait 5)
          (lambda () (step-hover (1+ i))))))))

(when noninteractive
  (deferred:$
    (step-hover 0)
    (deferred:nextc it
      (lambda (x) (message "all done::: %s" x) (kill-emacs t)))))


;; (profiler-write-profile
;;  (profiler-cpu-profile)
;;  "~/run-cpu.profile"
;;  nil)

;; (profiler-write-profile
;;  (profiler-memory-profile)
;;  "~/run-mem.profile"
;;  nil)

;; (profiler-write-profile profiler-report-profile "~/run.log" t)
