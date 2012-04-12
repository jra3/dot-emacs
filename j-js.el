(require 'cl)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; =============================================================================



;; ;; add buffer-local indicator for whether prog-mode-hook has run.
;; (defun my-set-pmh-ran ()
;;   (set (make-local-variable 'my-pmh-ran) t))

;; ;; prog-mode is emacs 24?
;; ;;(add-hook 'prog-mode-hook 'my-set-pmh-ran)

;; (add-hook 'js2-mode-hook 'my-run-pmh-if-not-ran)
;; (defun my-run-pmh-if-not-ran ()
;;   (unless (bound-and-true-p my-pmh-ran)
;;     (run-hooks 'prog-mode-hook)))

(add-hook 'js2-mode-hook 'my-disable-indent-tabs-mode)
(defun my-disable-indent-tabs-mode ()
  (set-variable 'indent-tabs-mode nil))

(eval-after-load "autopair"
  '(progn
     (autopair-global-mode 1)

     (setq my-autopair-off-modes '(js2-mode))
     (dolist (m my-autopair-off-modes)
       (add-hook (intern (concat (symbol-name m) "-hook"))
                 #'(lambda () (setq autopair-dont-activate t))))
     ))

(eval-after-load "autopair"
  '(progn
     (autopair-global-mode 1)))

(eval-after-load "js2-mode"
  '(progn
     (setq js2-mirror-mode nil)))

(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 2) ; 2 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)

     ;; following is from http://www.emacswiki.org/emacs/Js2Mode
     (add-hook 'js2-post-parse-callbacks 'my-js2-parse-global-vars-decls)
     (defun my-js2-parse-global-vars-decls ()
       (let ((btext (replace-regexp-in-string
                     ": *true" " "
                     (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
         (setq js2-additional-externs
               (split-string
                (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                " *, *" t))
         ))
     ))

;; =============================================================================

(require 'highlight-80+)
(add-hook 'js2-mode-hook (lambda () (highlight-80+-mode t)))

(provide 'j-js)
