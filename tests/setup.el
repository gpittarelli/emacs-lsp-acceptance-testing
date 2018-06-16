;;; setup.el --- Common setup for LSP mode testing -*- lexical-binding: t; -*-

(setq debug-on-error t)

(when noninteractive
  ;; Skip the first call of kill-emacs in batch mode -- it's emacs
  ;; trying to kill us cause it thinks we're done.
  (setq orig-kill-emacs (symbol-function 'kill-emacs))
  (defun kill-emacs (&rest yay)
    (fset 'kill-emacs orig-kill-emacs)))

(setq repodir (expand-file-name ".." (file-name-directory
                                      (or load-file-name (buffer-file-name)))))

(push (file-truename (concat repodir "/servers/javascript-typescript-langserver-npm/node_modules/.bin/")) exec-path)
(push (file-truename (concat repodir "/servers/typescript-language-server-npm/node_modules/.bin/")) exec-path)
(push (file-truename (concat repodir "/servers/vscode-css-languageserver-bin-npm/node_modules/.bin/")) exec-path)
(push (file-truename (concat repodir "/servers/dummy/")) exec-path)

(let ((default-directory  (expand-file-name "emacs/" repodir)))
  (normal-top-level-add-subdirs-to-load-path))

(require 'deferred)
(require 'lsp-mode)
(require 'lsp-javascript-typescript)
(require 'lsp-javascript-flow)
(require 'lsp-typescript)
(require 'lsp-go)
(require 'lsp-css)
(require 'lsp-haskell)
(require 'lsp-rust)
(require 'lsp-python)
(require 'lsp-java)
(require 'lsp-php)
(require 'lsp-groovy)
(require 'lsp-ruby)

(unless noninteractive
  (require 'lsp-ui))

(require 'js)
(require 'scss-mode)
(require 'haskell-mode)
(require 'rust-mode)
(require 'go-mode)
(require 'company)
(require 'pos-tip)
(require 's)
(require 'dash)
(require 'company-quickhelp)
(require 'company-lsp)
(require 'flycheck)
(require 'lsp-ui-flycheck)
(require 'groovy-mode)

(defmacro comment (&rest body) nil)

(push 'company-lsp company-backends)

(defvar my-show-pos-tip t)

;; autotesting in --batch: easy hooking into eldoc-message
(when noninteractive
  (setq eldoc-result nil)
  (defun eldoc-message (&rest args)
    (setq eldoc-result args))
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

(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
			 '("R" "--quiet" "--slave" "-e" "languageserver::run()"))
(add-hook 'R-mode-hook #'lsp-R-enable)

(lsp-define-stdio-client lsp-dummy "javascript"
                         (lambda () default-directory)
			 '("showMessage.sh"))


(defun my-groovy-mode-setup ()
  (company-mode)
  (lsp-groovy-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
    (lsp-ui-mode)))
(add-hook 'groovy-mode-hook #'my-groovy-mode-setup)

(defun my-js-mode-setup ()
  (company-mode)
  ;;(lsp-javascript-typescript-enable)
  ;;(lsp-javascript-typescript-enable)
  ;;(lsp-javascript-flow-enable)
  (eldoc-mode t)
  (flycheck-mode)
  ;; (unless noninteractive
  ;;   (setq-local eldoc-message-function #'my-eldoc-display-message))
  )

(defun my-rust-mode-setup ()
  (company-mode)
  (lsp-rust-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
    (lsp-ui-mode)))

(defun my-ruby-mode-setup ()
  (company-mode)
  (lsp-ruby-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
    (lsp-ui-mode)))

(defun my-css-mode-setup ()
  (when (eq major-mode 'css-mode)
    (company-mode)
    (lsp-css-enable)
    (eldoc-mode t)
    (flycheck-mode)
    (unless noninteractive
      (lsp-ui-mode))))

(defun my-scss-mode-setup ()
  (company-mode)
  (lsp-scss-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
    (lsp-ui-mode)))

(defun my-hs-mode-setup ()
  (company-mode)
  (lsp-haskell-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
    (setq-local eldoc-message-function #'my-eldoc-display-message)))

(defun my-go-mode-setup ()
  (company-mode)
  (lsp-go-enable)
  (eldoc-mode t)
  (flycheck-mode)
  (unless noninteractive
    (setq-local eldoc-message-function #'my-eldoc-display-message)))

(defun my-hide-pos-tip ()
  (setq my-show-pos-tip nil)
  (pos-tip-hide))

(defun my-unhide-pos-tip ()
  (setq my-show-pos-tip t))

(add-hook 'focus-out-hook #'my-hide-pos-tip)
(add-hook 'focus-in-hook #'my-unhide-pos-tip)

(add-hook 'js-mode-hook #'my-js-mode-setup)
(add-to-list 'auto-mode-alist '("\\.tsx?" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?" . js-mode))

(add-hook 'rust-mode-hook #'my-rust-mode-setup)
(add-to-list 'auto-mode-alist '("\\.rs" . rust-mode))

(add-hook 'go-mode-hook #'my-go-mode-setup)
(add-to-list 'auto-mode-alist '("\\.go" . go-mode))

(add-hook 'haskell-mode-hook #'my-hs-mode-setup)
(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))

(add-hook 'css-mode-hook #'my-css-mode-setup)
(add-hook 'scss-mode-hook #'my-scss-mode-setup)

(add-hook 'ruby-mode-hook #'my-ruby-mode-setup)

(define-key js-mode-map (kbd "M-.") #'xref-find-definitions)

;; Debugging:
;;(setq lsp-print-io t)
;;(setq lsp-print-io nil)

(setq output "output-unnamed.json")

(defun strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun log (hover-time completion-time completions)
  (let ((msg `(:y
               ,(line-number-at-pos)
               :x ,(current-column)
               :completions ,(mapcar 'strip-text-properties completions)
               :char ,(char-after)
               :time ,hover-time
               :completion-time ,completion-time
               :doc ,eldoc-result
               :code-actions ,lsp-code-actions
               :code-lenses ,lsp-code-lenses)))
    (with-current-buffer output
      (insert (json-encode msg))
      (insert "\n"))))

(defun advance (&optional first-time)
  (deferred:next
    (lambda ()
      (unless (eobp)
        (setq eldoc-result nil)
        (unless first-time
          (forward-char))
        (funcall eldoc-documentation-function)
        (setq prev-time (current-time))
        (step-hover 0)))))

(defun step-hover (i)
  (deferred:next
    (lambda ()
      (if (and (or eldoc-result (> i 200)))
          (let* ((hover-time (float-time (time-subtract (current-time) prev-time)))
                 (start-completion-time (current-time))
                 (completions (all-completions "" (caddr (lsp--get-completions))))
                 (completion-time (float-time (time-subtract (current-time) start-completion-time))))
            (log hover-time completion-time completions)
            (advance))

        (deferred:nextc
          (deferred:wait 0.10)
          (lambda () (step-hover (1+ i))))))))

(defun run-test (name)
  (let ((filename (concat "lsp-" name ".json")))
    (setq output (create-file-buffer filename))
    (setq prev-time (current-time))

    (when (gethash "codeLensProvider" (lsp--server-capabilities))
      (setq lens-start-time (current-time))
      (lsp--update-code-lenses))

    (deferred:$
      (advance t)
      (deferred:nextc it
        (lambda (x)
          (message "all done::: %s" x)
          (with-current-buffer output
            (write-file (file-truename (concat repodir "/" filename))))
          (kill-emacs t))))))

(lsp-define-stdio-client lsp-show-message-test "javascript"
                         (lambda () default-directory)
                         '("showMessage.sh"))

(comment
 (profiler-start 'cpu+mem)

 (progn
   (profiler-write-profile
    (profiler-cpu-profile)
    "~/run-cpu.profile"
    nil)

   (profiler-write-profile
    (profiler-memory-profile)
    "~/run-mem.profile"
    nil)

   (profiler-stop))
 )
