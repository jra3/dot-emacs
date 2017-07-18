(require 'lsp-flow)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))



;; Creative....
(font-lock-add-keywords
 'web-mode `(("\\(function *\\)("
             (0 (progn (compose-region (match-beginning 1) (match-end 1) "ƒ")
                       nil)))))

(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))



(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

;; =============================================================================

(provide 'j-js)
