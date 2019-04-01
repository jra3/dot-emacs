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
 '(newsticker-date-format "(%A %D %H:%M)" t)
 '(newsticker-html-renderer (quote shr-render-region) t)
 '(newsticker-use-full-width nil t)
 '(package-selected-packages
   (quote
    (thrift which-key hgrc-mode hgignore-mode gitconfig-mode gitignore-mode web-mode js-comint hack-mode yaml-mode google-c-style rainbow-mode package-lint rainbow-delimiters elisp-slime-nav cquery lsp-ui company-lsp lsp-mode flycheck git-gutter multiple-cursors tangotango-theme expand-region ag anzu ibuffer-git ibuffer-vc win-switch popwin yasnippet helm-xref helm-descbinds helm-flx flx exec-path-from-shell auto-package-update use-package)))
 '(shr-width 80))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
