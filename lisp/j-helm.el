(require 'helm)
(require 'helm-config)

;; M-x describe-bindings is now helm-y
(require 'helm-descbinds)
(helm-descbinds-mode)

;; Sweet version of M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)

;; Visual kill-ring
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; Way better find-files. C-x 4 f is already bound somehow...
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(yas-global-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Figure out how to set ag as default grepper?
;; (when (executable-find "ack-grep")
;;   (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
;;         helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(setq ;helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-autoresize-mode                  t
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
(provide 'j-helm)
