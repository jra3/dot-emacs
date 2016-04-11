(require 'cl)

(setq-default js2-auto-indent-p t)
(setq-default js2-cleanup-whitespace t)
(setq-default js2-enter-indents-newline t)

(setq-default js2-indent-on-enter-key t)
(setq-default js2-mode-indent-ignore-first-tab t)

(setq js2-basic-offset 4
      js2-bounce-indent-p nil)


(setq-default js2-global-externs '("$" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "localStorage" "ActiveXObject"))

;; We'll let fly do the error parsing...
(setq-default js2-show-parse-errors nil)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Creative....
(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
             (0 (progn (compose-region (match-beginning 1) (match-end 1) "ƒ")
                       nil)))))

;; (add-hook 'js2-mode-hook 'color-identifiers-mode)

(autoload 'flymake-jshint "flymake-jshint"
  "Error and linting support mode for JavaScript." t nil)

(add-hook 'js-mode-hook
          (lambda () (flymake-mode 1)))
(add-hook 'js-mode-hook 'flymake-jshint-load)


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

;; (add-hook 'js2-mode-hook 'my-disable-indent-tabs-mode)
;; (add-hook 'js2-mode-hook
;;           (lambda () (flymake-mode 1)))

;; (defun my-disable-indent-tabs-mode ()
;;   (set-variable 'indent-tabs-mode nil))

;; (eval-after-load "autopair"
;;   '(progn
;;      (autopair-global-mode 1)

;;      (setq my-autopair-off-modes '(js2-mode))
;;      (dolist (m my-autopair-off-modes)
;;        (add-hook (intern (concat (symbol-name m) "-hook"))
;;                  #'(lambda () (setq autopair-dont-activate t))))
;;      ))

;; (eval-after-load "autopair"
;;   '(progn
;;      (autopair-global-mode 1)))

;; (eval-after-load "js2-mode"
;;   '(progn
;;      (setq js2-mirror-mode nil)))

;; (eval-after-load "js2-mode"
;;   '(progn
;;      (setq js2-missing-semi-one-line-override t)
;;      (setq-default js2-basic-offset 2) ; 2 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)

;;      ;; following is from http://www.emacswiki.org/emacs/Js2Mode
;;      (add-hook 'js2-post-parse-callbacks 'my-js2-parse-global-vars-decls)
;;      (defun my-js2-parse-global-vars-decls ()
;;        (let ((btext (replace-regexp-in-string
;;                      ": *true" " "
;;                      (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
;;          (setq js2-additional-externs
;;                (split-string
;;                 (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
;;                 " *, *" t))
;;          ))
;;      ))

;; ;; =============================================================================

;; (require 'highlight-80+)
;; (add-hook 'js2-mode-hook (lambda () (highlight-80+-mode t)))

(provide 'j-js)
