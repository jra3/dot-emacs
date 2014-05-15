(require 'thrift)

(setq auto-mode-alist (append '(("\\.thrift$" . thrift-mode))
                              auto-mode-alist))

(provide 'j-thrift)
