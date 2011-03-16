(require 'flymake)
(require 'j-flymake)
(require 'diminish)

;; set code checker here from "epylint", "pyflakes"
(setq pycodechecker "pyflakes")
(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)
            (setq require-final-newline t)
            (setq mode-name "Py")
            (local-set-key "\C-c\C-r" 'revert-buffer)
            (c-set-offset  'arglist-intro '+)))

(provide 'j-python)
