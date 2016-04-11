(require 'company) ;; cool for clang
(setq company-backends
      (quote
       (company-bbdb
        company-nxml
        company-css
        company-semantic
        company-clang
        company-xcode
        company-cmake
        company-capf
        (company-dabbrev-code
         company-gtags
         company-etags)
        company-oddmuse
        company-files
        company-dabbrev)))

(require 'yasnippet)
;; (yas-installed-snippets-dir
;; (setq yas-snippet-dirs (quote ("~/.dot-emacs/snippets")) nil (yasnippet))

(provide 'j-completion)
