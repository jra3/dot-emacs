(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

(require 'go-dlv)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(defun string/starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
	 (string-equal (substring s 0 (length begins)) begins))
	(t nil)))

(defun go-oracle-set-main ()
  "set the scope to the current main package"
  (interactive)

  (let d (expand-file-name (pwd))
       (message d)
       (if (string/starts-with d (concat (getenv "GOPATH") "src/"))
	   (message "win"))
       )
  )

;;;; Golang support
(defun my-go-mode-hook ()
  (whitespace-mode -1) ; don't highlight hard tabs
  (local-set-key (kbd "M-.") 'godef-jump)
  (auto-complete-mode 1)

   ; Go oracle
  (load-file "~/go/src/golang.org/x/tools/cmd/oracle/oracle.el")

  (add-hook 'before-save-hook 'gofmt-before-save)

  (setq compile-command "go build -v && go test -v && go vet && golint")

  (subword-mode 1)
  
  (setq
   gofmt-command "goimports"
   tab-width 2         ; display tabs as two-spaces
   indent-tabs-mode 1  ; use hard tabs to indent
   fill-column 100)    ; set a reasonable fill width
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(require 'go-autocomplete)
(require 'auto-complete-config)

;; ;;Configure golint
;; (add-to-list 'load-path "~/go/src/github.com/golang/lint/misc/emacs")
;; (require 'golint)

;; (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;; (require 'go-flymake)

(define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)

(provide 'j-go)
