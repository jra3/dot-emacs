;; (require 'php-mode)
;; (require 'xhp-mode)
;; (require 'highlight-80+)

(require 'js2-mode)
(require 'xhp-mode)
(require 'hack-mode)
(require 'highlight-80+)

(defun ywb-php-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun ywb-php-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(defconst fb-php-style
  '((c-basic-offset . 2)
    (c-offsets-alist . ((substatement-open . 0)
                        )))
  "Facebook's PHP Programming style")
(c-add-style "fb-php-style" fb-php-style)

(let
    ((mode-hook (lambda ()
                  (c-set-style "fb-php-style")
                  (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
                  (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)
                  
                  (highlight-80+-mode t)
                  (set (make-local-variable 'require-final-newline) t)
                  )))

  (add-hook 'xhp-mode-hook mode-hook)
  (add-hook 'hack-mode-hook mode-hook)
)

(provide 'j-php)
