(require 'php-mode)
(require 'xhp-mode)
(require 'highlight-80+)
(defun jallen-php-compile-command ()
  "Set a buffer local compile command if buffer is under flib"
  (set (make-local-variable 'compile-command)
       (let ((is-in-flib (string-match "^.*/flib/" buffer-file-name)))
         (let (
               (checkModule-flags (cond (is-in-flib "-w") ("-w -s")))
               (module-or-path
                (cond (is-in-flib
                       (replace-match "" nil nil
                                      (file-name-directory buffer-file-name)))
                      (buffer-file-name))))
           (format "cd /home/jallen/www && /home/jallen/www/flib/_bin/checkModule --emacs %s %s"
                   checkModule-flags
                   module-or-path)))
       ))

(add-hook 'php-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            (define-key php-mode-map (kbd "M-g") 'tbgs)
            (local-set-key (kbd "RET") 'c-context-line-break)
            (local-set-key (kbd "C-c a") 'php-array-align)
            (c-set-offset 'case-label 2)
            (c-set-offset  'arglist-intro '+)
            (setq require-final-newline t)
            ))

(add-hook 'php-mode-hook 'jallen-php-compile-command)

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
            ))

(add-hook 'xhp-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ))

(provide 'j-php)
