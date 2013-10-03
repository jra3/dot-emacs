;; working with fbcode
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(require 'j-completion)
(require 'highlight-80+)
(require 'google-c-style)

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
