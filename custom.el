;;; custom.el --- Junk custom file                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  John Allen

;; Author: John Allen <jallen@devvm327.frc2.facebook.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-tasks company-reviewers company-bbdb company-nxml company-css company-capf
                   (company-dabbrev-code company-keywords))))
 '(company-minimum-prefix-length 1)
 '(lsp-highlight-symbol-at-point t t)
 '(newsticker-date-format "(%A %D %H:%M)" t)
 '(newsticker-html-renderer (quote shr-render-region) t)
 '(newsticker-use-full-width nil t)
 '(package-selected-packages
   (quote
    (dap-mode cquery git-gutter hack-mode elpa-mirror lsp-hack package-lint company-lsp lsp-ui lsp-mode which-key hgrc-mode hgignore-mode w3 w3m flycheck multiple-cursors helm-descbinds-mode load-dir yaml-mode win-switch whitespace-cleanup-mode web-mode wanderlust use-package thrift tangotango-theme rainbow-mode rainbow-delimiters popwin org-pomodoro minimap markdown-mode magit json-mode js2-refactor js-comint ibuffer-vc ibuffer-git htmlize helm-unicode helm-pydoc helm-projectile helm-orgcard helm-flycheck helm-flx helm-describe-modes helm-descbinds helm-css-scss helm-company helm-c-yasnippet google-c-style gitignore-mode gitconfig-mode exec-path-from-shell elpy elisp-slime-nav dired-details delight company-jedi color-identifiers-mode auto-complete anzu ag)))
 '(shr-width 80))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
