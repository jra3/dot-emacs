;;; package --- jallen's .emacs file
;; -*- Emacs-Lisp -*-

;;; Commentary:
;;  he's so dreamy

;;; Code:

(if (file-exists-p "~/.emacs.d/local.el")
    (load "~/.emacs.d/local.el"))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.local/bin")

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/lisp")
           (default-directory my-lisp-dir))
      (add-to-list 'load-path my-lisp-dir)
      (normal-top-level-add-subdirs-to-load-path)))

(require 'package)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))

(defvar package-list
  '(
    lsp-mode
    lsp-python

    js2-mode
    ag  ; the silver searcher!
    anzu
    bbdb
    company
    color-identifiers-mode
    smex
    thrift
    diminish
    elpy
    ;; crontab-mode
    gitconfig-mode
    gitignore-mode
    htmlize

    rtags
    helm-rtags
    company-rtags
    company-flow

    auto-complete
    exec-path-from-shell

    flycheck
    flycheck-rtags

    go-mode
    go-eldoc
    go-autocomplete
    go-rename
    go-guru

    helm-c-yasnippet
    helm-company
    helm-css-scss
    helm-descbinds
    helm-describe-modes
    helm-flx
    helm-flycheck
    helm-orgcard
    helm-projectile
    helm-pydoc
    helm-unicode

    markdown-mode

    js-comint
    js2-refactor
    json-mode
    coffee-mode

    yaml-mode
    google-c-style
    multiple-cursors
    magit
    nose
    popwin
    projectile
    rainbow-mode
    rainbow-delimiters
    tangotango-theme
    web-mode

    whitespace-cleanup-mode
    wanderlust
    ))

(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'j-ediff)
(require 'j-terminal-compat)
(require 'j-mc)
(require 'j-util)
(require 'j-globalkeys)
(require 'j-colors)
(require 'j-appearance)
(require 'j-behavior)
(require 'j-org)

(require 'j-go)
(require 'j-cpp)
(require 'j-php)
(require 'j-python)
(require 'j-js)
(require 'j-html)
(require 'j-css)
(require 'j-thrift)
(require 'j-yaml)
(require 'j-markdown)

(require 'j-helm)
(require 'j-completion)
(require 'j-web)

(require 'flow)

(require 'helm-myles)

(if (file-exists-p "~/.local-overrides.el")
    (load-file "~/.local-overrides.el"))

;;; .emacs ends here
