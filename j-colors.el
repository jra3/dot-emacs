(require 'color-theme)
(color-theme-initialize)

(require 'color-theme-tangotango)

;; select theme - first list element is for windowing system, second is for console/terminal
;; Source : http://www.emacswiki.org/emacs/ColorTheme#toc9
(setq color-theme-choices '(color-theme-tangotango color-theme-dark-laptop))

;; test for each frame or console
(require 'cl)
(lexical-let ( (cols color-theme-choices) )
  (defun test-win-sys (frame)
    (let ( (color-theme-is-global nil) )
      (select-frame frame)
      (eval (append '(if (window-system frame))
                    (mapcar (lambda (x) (cons x nil)) cols))))))
;; hook on after-make-frame-functions
(add-hook 'after-make-frame-functions 'test-win-sys)

(provide 'j-colors)
