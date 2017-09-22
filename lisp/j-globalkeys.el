(windmove-default-keybindings)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "C-c =") 'align-eq)
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-c t")  'goto-line)

(global-set-key
 (kbd "C-x <right>")
 '(lambda () (interactive) (enlarge-window-horizontally 4)))

(global-set-key
 (kbd "C-x <left>")
 '(lambda () (interactive) (shrink-window-horizontally 4)))

(global-set-key
 (kbd "C-x <up>")
 '(lambda () (interactive) (enlarge-window 4)))

(global-set-key
 (kbd "C-x <down>")
 '(lambda () (interactive) (shrink-window 4)))

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Diff the current buffer with the file contents
(global-set-key (kbd "C-c w")
                (lambda () (interactive)
                  (diff-buffer-with-file (current-buffer))))

(require 'multiple-cursors)
(global-set-key (kbd "M-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; This is a little silly, but I'll leave it here as a fun example
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
;; (global-set-key (kbd "C-c +") 'increment-number-at-point)

(provide 'j-globalkeys)
