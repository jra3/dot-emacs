(require 'php-mode)
(require 'xhp-mode)
(require 'highlight-80+)


;; run php lint when press f8 key
(defun phplint-thisfile ()
  (interactive)
  (compile (format "/usr/local/bin/php -l %s" (buffer-file-name))))

(add-hook 'php-mode-hook
          '(lambda ()
             (local-set-key [f8] 'phplint-thisfile)))

(add-hook 'php-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            (subword-mode 1)
            (define-key php-mode-map (kbd "M-g") 'tbgs)
            (local-set-key (kbd "RET") 'c-context-line-break)
            (local-set-key (kbd "C-c a") 'php-array-align)
            (local-set-key (kbd "C-M-T") 'pfff-infer-type-at-point)
            (c-set-offset 'case-label 2)
            (c-set-offset  'arglist-intro '+)
            (setq require-final-newline t)
            ))

;; PHP mode for .phpt files
(autoload 'php-mode "php-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.phpt$" . php-mode))
                              auto-mode-alist))
(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)

;; Set PHP mode based on the #! line
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

(defconst fb-php-style
  '((c-basic-offset . 2)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        )))
  "Facebook's PHP Programming style"
  )
(c-add-style "fb-php-style" fb-php-style)

(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            (visit-tags-table "~/www/TAGS")
            ))

(add-hook 'xhp-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            (visit-tags-table "~/www/TAGS")
            ))

(provide 'j-php)
