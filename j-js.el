(require 'cl)

(setq-default js2-auto-indent-p t)
(setq-default js2-cleanup-whitespace t)
(setq-default js2-enter-indents-newline t)

(setq-default js2-indent-on-enter-key t)
(setq-default js2-mode-indent-ignore-first-tab t)

(setq js2-basic-offset 2
      js2-bounce-indent-p nil)


(setq-default js2-global-externs '("$" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "localStorage" "ActiveXObject"))

;; We'll let fly do the error parsing...
(setq-default js2-show-parse-errors nil)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Creative....
(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
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

;; =============================================================================

(provide 'j-js)
