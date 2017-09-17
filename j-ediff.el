;;; j-ediff.el --- customizations for ediffs         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  John Allen

;; Author: John Allen <jallen@devvm327.frc2.facebook.com>
;; Keywords: abbrev, abbrev, abbrev, files,

(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; Usage: emacs -diff file1 file2

(add-hook 'ediff-load-hook
          (lambda ()
            (message "getting my diff onnnnn")
            (flycheck-mode nil)
            (setq ediff-highlight-all-diffs nil)
            (set-face-background
             ediff-current-diff-face-A "#1e2424")
            (set-face-background
             ediff-current-diff-face-B "#1e2424")
            (set-face-background
             ediff-current-diff-face-C "#1e2424")
            (make-face-italic
             ediff-current-diff-face-A)
            (make-face-italic
             ediff-current-diff-face-B)
            (make-face-italic
             ediff-current-diff-face-C)))

(provide 'j-ediff)
