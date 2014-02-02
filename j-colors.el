(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-tangotango)

;;
;; Note: Make sure iTerm2 is configured to use xterm-256colors
;;
;; http://rtlechow.com/2008/12/15/256-colors-in-vim-inside-screen-in-an-iterm-on-os-x-leopard/
;;
;; Tested:
;;   emacs run in iTerm w 256 colors
;;   emacsclient in iTerm attached to daemon -- lighter background than direct emacs???
;;   Cocoa app
;;

;; Set up hooks to create frames with correct color schemes
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
  `after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
(add-hook 'after-init-hook
          (lambda ()
            (run-after-make-frame-hooks (selected-frame))))

(add-hook 'after-make-window-system-frame-hooks 'color-theme-tangotango)
(add-hook 'after-make-console-frame-hooks 'color-theme-tangotango)

;; Because if statements!!
(if window-system
    (color-theme-tangotango)
  (color-theme-tangotango))

(set-variable 'color-theme-is-global nil)

(provide 'j-colors)
