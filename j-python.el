(require 'diminish)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key [f9] 'pytidy-whole-buffer)
;;             ;;(flymake-mode t)
;;             (setq require-final-newline t)
;;             (c-set-offset  'arglist-intro '+)))

;; ;; PythonTidy does some annoying shit...
;; (defun pytidy-whole-buffer ()
;;   (interactive)
;;   (let ((a (point)))
;;     (shell-command-on-region (point-min) (point-max) "PythonTidy.py" t)
;;     (goto-char a)))

(elpy-enable)
(elpy-clean-modeline)
(elpy-use-ipython)

;; Fixing a key binding bug in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(provide 'j-python)
