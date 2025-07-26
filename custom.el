;;; custom.el --- Junk custom file                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  John Allen

;; Author: John Allen

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
 '(custom-enabled-themes '(tangotango))
 '(custom-safe-themes
    '("cd69d46df6559baf9e26b47e93c0bf69b7c8d32db079c3c105d66e38e17c8cdf"
       default))
 '(diary-file "~/diary")
 '(lsp-javascript-display-return-type-hints t)
 '(lsp-javascript-display-variable-type-hints t)
 '(lsp-restart 'auto-restart)
 '(lsp-typescript-implementations-code-lens-enabled t)
 '(lsp-typescript-references-code-lens-enabled t)
 '(lsp-typescript-suggest-complete-function-calls t)
 '(org-agenda-current-time-string "> you are here <")
 '(org-agenda-hide-tags-regexp "TASKS\\|WAITING\\|SOMEDAY\\|HOLD\\|TICKLER")
 '(org-agenda-include-diary nil)
 '(org-agenda-sorting-strategy
    '((agenda habit-down effort-up time-up priority-down category-keep)
       (todo priority-down category-keep)
       (tags priority-down category-keep) (search category-keep)))
 '(org-agenda-time-grid
    '((daily today require-timed remove-match)
       (900 1000 1100 1200 1300 1400 1500 1600 1700 1800) "......"
       "----------------"))
 '(org-agenda-timegrid-use-ampm nil)
 '(org-archive-location "~/org/archive/archive.org::* From %s")
 '(org-clock-task-overrun-text "Don't be such a punk")
 '(org-export-with-toc nil)
 '(org-pomodoro-format "P~%s")
 '(org-pomodoro-play-sounds nil)
 '(org-special-ctrl-a/e 'reversed)
 '(package-selected-packages
    '(2048-game add-node-modules-path anzu apheleia auto-package-update
       button-lock cargo claude-code company corfu cython-mode eat
       elisp-slime-nav embark-consult exec-path-from-shell
       expand-region flycheck-golangci-lint git-gutter go-eldoc
       go-guru go-tag google-c-style gotest helm-descbinds helm-flx
       helm-xref ibuffer-vc jade-mode jest lsp-ui marginalia
       modern-cpp-font-lock multiple-cursors nodejs-repl npm-mode
       orderless org-pomodoro package-lint popwin protobuf-mode
       rainbow-delimiters rainbow-mode rust-mode tangotango-theme tide
       toml-mode tree-sitter-langs vertico vterm win-switch yaml-mode
       yasnippet))
 '(shr-width 80)
 '(tab-width 2)
 '(undo-outer-limit 120000000))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((t (:foreground "#ad7fa8" :slant normal)))))
