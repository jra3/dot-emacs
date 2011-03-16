(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-tangotango)

(color-theme-dark-laptop)
(defun apply-color-theme (frame)
  "Apply color theme to a frame based on whether its a 'real'
    window or a console window."
  (let ((color-theme-is-global nil))    
    (select-frame frame)
    (if (window-system frame)
	(color-theme-tangotango)
      (color-theme-dark-laptop))))

(add-hook 'after-make-frame-functions 'apply-color-theme)

(provide 'j-colors)
