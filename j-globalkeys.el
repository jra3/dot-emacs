;; GLOBAL KEY BINDINGS
(global-set-key "\C-xx" nil)
(global-set-key "\C-xxb" 'git-blame-mode)
(global-set-key "\C-xxs" 'git-status)
(global-set-key "\C-xxm" 'magit-status)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [C-delete] 'kill-word)

(global-set-key [C-home] 'beginning-of-buffer)
(global-set-key [C-end] 'end-of-buffer)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [delete] 'delete-char)

(global-set-key (kbd "C-c =") 'eq-align)

(global-set-key "\M-'" 'tags-apropos)

(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-t"  'goto-line)
(global-set-key "\C-o"  'query-replace)
(global-set-key "\M-o"  'replace-string)
(global-set-key "\C-\\" 'indent-region)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\M->" 'next-tag-definition)

(global-set-key (kbd "C-x <right>") 'grow-horiz)
(global-set-key (kbd "C-x <left>")  'shrink-horiz)
(global-set-key (kbd "C-x <up>")    'grow-vert)
(global-set-key (kbd "C-x <down>")  '(lambda () (interactive)
                                       (shrink-window 4)))
(windmove-default-keybindings)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Diff the current buffer with the file contents
(global-set-key (kbd "C-c w")
   (lambda () (interactive) (diff-buffer-with-file (current-buffer))))

(global-set-key [(f6)] 'compile)
(global-set-key [(f7)] 'next-error)

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

;; ----------------- Key Bindings ----------------------------
;; Make modifier+arrow-key work inside ttys
(when (not window-system)
  (dolist (prefix '("\eO" "\eO1;" "\e[1;"))
    (dolist (m '(("2" . "S-") ("3" . "M-") ("4" . "S-M-") ("5" . "C-")
                 ("6" . "S-C-") ("7" . "C-M-") ("8" . "S-C-M-")))
      (dolist (k '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>")
                   ("D" . "<left>") ("H" . "<home>") ("F" . "<end>")))
        (define-key function-key-map
          (concat prefix (car m) (car k))
          (read-kbd-macro (concat (cdr m) (cdr k)))))
      )
    ))

(provide 'j-globalkeys)
