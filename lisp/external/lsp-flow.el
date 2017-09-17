(require 'lsp-mode)
(require 'lsp-common)

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck))
(lsp-flycheck-add-mode 'flow)

(defgroup lsp-flow nil
  "`lsp-flow' for using LSP server while editing flow code."
  :prefix "lsp-flow-")

(defcustom lsp-flow-binary
  "flow-language-server"
  "*The binary that runs the flow LSP server."
  :group 'lsp-flow
  :type 'string
  )

(lsp-define-stdio-client 'web-mode "flow" 'stdio
                         ;; find the project root by looking for the .hhconfig file
                         (lsp-make-traverser #'(lambda (dir)
                                                 (directory-files
                                                  dir
                                                  nil
                                                  "\\.flowconfig")))
                         "Flow Language Server"
                         (list "/usr/bin/node" lsp-flow-binary "--stdio")
                         )

(provide 'lsp-flow)
