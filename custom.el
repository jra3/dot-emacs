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
 '(auto-package-update-delete-old-versions nil)
 '(bbdb-file-remote "/usr/share/emacs/bbdb")
 '(company-backends
   (quote
    (company-tasks company-reviewers company-bbdb company-nxml company-css company-capf
                   (company-dabbrev-code company-keywords))))
 '(company-minimum-prefix-length 1)
 '(lsp-restart (quote auto-restart))
 '(newsticker-date-format "(%A %D %H:%M)" t)
 '(newsticker-html-renderer (quote shr-render-region) t)
 '(newsticker-use-full-width nil t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down effort-up time-up priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-archive-location "~/org/archive/archive.org::* From %s")
 '(org-clock-task-overrun-text "Don't be such a punk")
 '(org-pomodoro-format "P~%s")
 '(org-pomodoro-play-sounds nil)
 '(package-selected-packages
   (quote
    (lsp-mode org-pomodoro wttrin arduino-mode bbdb thrift yasnippet yaml-mode win-switch which-key web-mode use-package tangotango-theme rainbow-mode rainbow-delimiters popwin package-lint multiple-cursors lsp-ui lsp-hack js-comint ibuffer-vc ibuffer-git hgrc-mode hgignore-mode helm-xref helm-flx helm-descbinds hack-mode google-c-style gitignore-mode gitconfig-mode git-gutter flycheck expand-region exec-path-from-shell elisp-slime-nav cquery company-lsp auto-package-update anzu ag)))
 '(shr-width 80))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
