;;; compile-warnings-fix.el --- Reduce package compilation warnings -*- lexical-binding: t; -*-

;;; Commentary:
;; Strategies to reduce byte-compile warnings from third-party packages

;;; Code:

;; 1. Suppress warnings during package installation
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; 2. Native compilation settings (Emacs 28+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Suppress native comp warnings
  (setq native-comp-async-report-warnings-errors nil
        native-comp-warning-on-missing-source nil)
  
  ;; Set native comp verbosity to errors only
  (setq native-comp-verbose 0)
  
  ;; Reduce native comp message spam
  (setq native-comp-async-query-on-exit t
        native-comp-async-jobs-number 4))

;; 3. Warning suppression for specific issues
(with-eval-after-load 'warnings
  ;; Add specific warning types to suppress
  (add-to-list 'warning-suppress-types '(comp))
  (add-to-list 'warning-suppress-types '(bytecomp))
  (add-to-list 'warning-suppress-types '(lexical-binding)))

;; 4. Package-specific fixes
(defun my/suppress-package-warnings ()
  "Suppress warnings for specific packages."
  
  ;; For packages without lexical-binding
  (dolist (file '("win-switch" "multiple-cursors" "lv" "go-guru" 
                  "tangotango-theme" "spinner"))
    (with-eval-after-load file
      (setq-local byte-compile-warnings 
                  (remq 'lexical-binding byte-compile-warnings))))
  
  ;; For obsolete function warnings
  (with-eval-after-load 'multiple-cursors
    (setq-local byte-compile-warnings 
                (remq 'obsolete byte-compile-warnings))))

;; 5. Auto-compile settings to reduce noise
(when (require 'auto-compile nil t)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter nil
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t
        auto-compile-update-autoloads t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

;; 6. Quiet package.el
(setq package-native-compile t)  ; Use native compilation when available
(setq package-install-upgrade-built-in nil)  ; Don't show built-in package warnings

;; 7. Load path cleaning to avoid duplicate warnings
(defun my/clean-load-path ()
  "Remove duplicates from load-path."
  (setq load-path (cl-remove-duplicates load-path :test #'string=)))

(my/clean-load-path)

;; 8. Advice to suppress specific warning patterns
(defun my/suppress-compile-warnings (orig-fun &rest args)
  "Suppress specific warning patterns during compilation."
  (let ((warning-suppress-types (append warning-suppress-types 
                                        '((lexical-binding)
                                          (docstring)
                                          (obsolete)))))
    (apply orig-fun args)))

;; Apply advice during package operations
(advice-add 'package-install :around #'my/suppress-compile-warnings)
(advice-add 'package-compile :around #'my/suppress-compile-warnings)

;;; compile-warnings-fix.el ends here