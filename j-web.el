(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-enable-auto-closing t
      web-mode-enable-auto-indentation t
      web-mode-enable-auto-opening t
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-quoting t
      web-mode-markup-indent-offset 2
      web-mode-script-padding 2
      web-mode-style-padding 2)



(provide 'j-web)
