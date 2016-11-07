(require 'diminish)

(eval-after-load 'elpy
  '(progn
     (define-key elpy-mode-map (kbd "C-c C-f") 'helm-projectile)  ;; unshadow projectile-find-file
     (define-key elpy-mode-map (kbd "<C-down>") 'python-nav-forward-block)
     (define-key elpy-mode-map (kbd "<C-up>") 'python-nav-backward-block)
     (define-key elpy-mode-map (kbd "<C-left>") nil)
     (define-key elpy-mode-map (kbd "<C-right>") nil)
     (define-key elpy-mode-map (kbd "C-c C-s") 'helm-projectile-grep)  ;; elpy-rgrep-symbol sucks
))

(elpy-enable)
;(elpy-use-ipython)

(setq elpy-default-minor-modes (quote
                                (eldoc-mode flymake-mode yas-minor-mode auto-complete-mode))
      elpy-mode-hook (quote (subword-mode hl-line-mode))
      elpy-modules (quote
                    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults))
      elpy-project-ignored-directories (quote
                                        (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "third_party" "third_party_dev" "node_modules"))
      elpy-project-root-finder-functions (quote (elpy-project-find-projectile-root))
      elpy-rgrep-ignored-directories (quote (".tox" "build" "dist" "third_party"))
      elpy-rpc-backend "jedi"
      elpy-rpc-python-command "/opt/interana/third_party/bin/python2.7"
      elpy-test-nose-runner-command (quote ("nosetests" "-vs"))
      elpy-test-runner (quote elpy-test-pytest-runner))


(setq py-basic-offset 4)
;; Fixing a key binding bug in elpy
;;;;;;(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c o") 'iedit-mode)

(provide 'j-python)
