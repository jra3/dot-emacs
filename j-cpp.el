;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'j-completion)
(require 'highlight-80+)
(require 'google-c-style)

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(add-hook 'c++-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            (define-key c++-mode-map (kbd "C-c o") 'ff-get-other-file)
            (flymake-mode t)
            (subword-mode 1)
            (setq require-final-newline t)
            (setq compilation-auto-jump-to-first-error t)
            ))

;; cpplint.py courtesy of google
(setq cppcodechecker "cpplint.py")
(when (load "flymake" t)
  (defun flymake-cppcodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list cppcodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.cpp\\'" flymake-cppcodecheck-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.h\\'" flymake-cppcodecheck-init))
  )

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(provide 'j-cpp)
