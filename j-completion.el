;; Jedi for python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:server-command '("~/.emacs.d/el-get/jedi/jediepcserver.py"))

(require 'company) ;; cool for clang

(provide 'j-completion)
