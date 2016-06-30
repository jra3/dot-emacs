(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(defun my-go-mode-hook ()
					
  (setq gofmt-command "goimports") ; Use goimports instead of go-fmt
  (add-hook 'before-save-hook 'gofmt-before-save) ; Call Gofmt before saving

  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  
  ; Go oracle
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(provide 'j-go)
