;;; lsp-javascript.el --- Testing javascript LSP mode -*- lexical-binding: t; -*-

(setq output (create-file-buffer "output.json"))

(find-file (file-truename (concat repodir "/tests/fixtures/js/a.js")))

;; (profiler-start 'cpu+mem)

(defun log (time)
  (let ((msg `(:y ,(line-number-at-pos)
                  :x ,(current-column)
                  :char ,(char-after)
                  :time ,time
                  :doc ,abc
                  :code-actions ,lsp-code-actions
                  :code-lenses ,lsp-code-lenses)))
    (with-current-buffer output
      (insert (json-encode msg))
      (insert "\n"))))


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
      (if (and (or abc (> i 200)))
          (progn
            (when abc
              (message "At (%s,%s) in %sms on \"%s\": %s"
                       (line-number-at-pos) (current-column)
                       (* i 0.25)
                       (symbol-at-point)
                       abc))
            (log (* i 0.25))

            (when lsp-code-actions
              (message "found code actions for %s: %s" i (symbol-at-point) lsp-code-actions))

            (setq abc nil)
            (advance))
        (deferred:nextc
          (deferred:wait 0.25)
          (lambda () (step-hover (1+ i))))))))

(when noninteractive
  (deferred:$
    (step-hover 0)
    (deferred:nextc it
      (lambda (x)
        (message "all done::: %s" x)
        (with-current-buffer output
          (write-file "output.json"))
        (kill-emacs t)))))


;; (profiler-write-profile
;;  (profiler-cpu-profile)
;;  "~/run-cpu.profile"
;;  nil)

;; (profiler-write-profile
;;  (profiler-memory-profile)
;;  "~/run-mem.profile"
;;  nil)

;; (profiler-write-profile profiler-report-profile "~/run.log" t)
