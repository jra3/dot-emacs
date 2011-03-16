(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-tangotango)

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

(color-theme-dark-laptop)
(set-variable 'color-theme-is-global nil)
(add-hook 'after-make-window-system-frame-hooks 'color-theme-tangotango)
(add-hook 'after-make-console-frame-hooks 'color-theme-dark-laptop)

(provide 'j-colors)
