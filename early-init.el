;;; early-init.el --- Early initialization for Emacs 27+ -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI are initialized
;; Perfect for performance optimizations

;;; Code:

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil)  ; Emacs 28+ with native compilation

;; Disable some GUI elements for faster startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable package.el in favor of straight.el or manual management
(setq package-enable-at-startup nil)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq frame-inhibit-implied-resize t)

;; Suppress compilation warnings early
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq warning-minimum-level :error)

;; Native compilation settings
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-warning-on-missing-source nil)

;;; early-init.el ends here