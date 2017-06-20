;;; flow.el --- interface with flow binary for editing JS
;;
;; Copyright (c) 2016-present, Facebook, Inc.
;; All rights reserved.
;;
;; This source code is licensed under the BSD-style license found in the LICENSE
;; file in the root directory of this source tree. An additional grant of patent
;; rightscan be found in the PATENTS file in the same directory.
;;
;;; Commentary:
;;
;; Utilities for using flow typing in emacs
;;
;; Code:

(defcustom flow-binary "flow"
  "*Name of flow executable (must be in path)."
  :group 'flow
  :type 'string)

(defun flow-start ()
  (shell-command (format "%s start" flow-binary))
)

(defun flow-stop ()
  (shell-command (format "%s stop" flow-binary))
)

(defun flow-status ()
  "Initialize flow"
  (interactive)
  (flow-start)
  (compile (format "%s status --from emacs; exit 0" flow-binary))
)

(defun column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column))
)

(defun string-of-region ()
  "string of region"
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (format ":%d:%d,%d:%d"
                (line-number-at-pos begin)
                (column-number-at-pos begin)
                (line-number-at-pos end)
                (column-number-at-pos end)))
    "")
)

(defun flow-type-at-pos ()
  "show type"
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos))
        (col (current-column))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (flow-start)
    (shell-command
     (format "%s type-at-pos --from emacs %s %d %d"
             flow-binary
             file
             line
             (1+ col)))
    (compilation-mode)
    (switch-to-buffer-other-window buffer))
)

(defun flow-suggest ()
  "fill types"
  (interactive)
  (let ((file (buffer-file-name))
        (region (string-of-region))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (flow-start)
    (shell-command
     (format "%s suggest %s%s"
             flow-binary
             file
             region))
    (compilation-mode)
    (switch-to-buffer-other-window buffer))
)

(defun flow-get-def ()
  "jump to definition"
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos))
        (col (current-column))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (flow-start)
    (shell-command
     (format "%s get-def --from emacs %s %d %d"
             flow-binary
             file
             line
             (1+ col)))
    (compilation-mode))
)

(defun flow-autocomplete ()
  "autocomplete"
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos))
        (col (current-column))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (flow-start)
    (shell-command
     (format "%s autocomplete %s %d %d < %s"
             flow-binary
             file
             line
             (1+ col)
             file))
    (compilation-mode)
    (switch-to-buffer-other-window buffer))
  )

;;;###autoload
(define-minor-mode flow-mode
  "Get in the flow, and edit your JS with confidence."
  nil
  " Flow"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-m") 'flow-status)
    (define-key map (kbd "C-c C-t") 'flow-type-at-pos)
    (define-key map (kbd "  ") 'flow-suggest)
    (define-key map (kbd "M-TAB") 'flow-autocomplete)
    (define-key map (kbd "M-.") 'flow-get-def)
    map)

  (setq-local compile-command
              (format "%s status --from emacs; exit 0" flow-binary))

  ;; hook this
  (flow-start)
  )

(provide 'flow)
;;; flow.el ends here
