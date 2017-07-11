(require 'company)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends '((company-capf
                          company-files
                          company-elisp
                          ;; company-inf-ruby company-anaconda company-go company-irony company-clang
                          company-cmake
                          company-css
                          company-yasnippet)
                         (company-dabbrev company-dabbrev-code)))

(defvar my-reviewers
  '(
    "NT"

    "maxsegan"
    "henryz"
    "nahapetyan"
    "uts"
    "d14z"
    "rohanmehta"
    )
  )

(defun company-reviewers--engineers ()
  "returns a list of unixnames in the engineers unix group"
  (interactive)
  (split-string (shell-command-to-string "getent group engineers | cut -d: -f4") "," t))

(defun company-reviewers--annotation (s)
  (format
   " [%s]"
   (replace-regexp-in-string
    "\n$" ""
    (shell-command-to-string
     (format "getent passwd %s | cut -d: -f5" s))))
  )

(defun company-reviewers (command &optional arg &rest ignored)
  "`company-mode' completion backend for my common diff reviewers."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-reviewers))
    (prefix (and (memq major-mode '(text-mode))
                 (looking-back "^\\(Reviewers\\|Subsribers\\): *.*? *\\([^,]*\\)"
                               (line-beginning-position))
                 (match-string-no-properties 2)))
    (candidates (all-completions arg (append
                                      my-reviewers
                                      (company-reviewers--engineers))))
    (annotation (company-reviewers--annotation arg))
    (sorted t)
    (no-cache t)))

;; TODO defcustom this into an fb variable
(defvar company-tasks-username nil
  "Username to use for tasks completion. when nil, $USER is used")

(defun company-tasks--make-candidate (candidate)
  (let ((task_number (cdr (assoc 'task_number candidate)))
        (annotation (cdr (assoc 'title candidate)))
        (meta (cdr (assoc 'description candidate)))
        )
    (propertize task_number
                'annotation annotation
                'meta meta)))

(defun company-tasks--candidates (prefix)
  (mapcar
   'company-tasks--make-candidate
   (json-read-from-string (shell-command-to-string "/usr/local/bin/tasks search --name jallen --json"))))

(defun company-tasks--annotation (candidate)
  (format " %s" (get-text-property 0 'annotation candidate)))

(defun company-tasks--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-tasks (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tasks))
    (prefix (and (memq major-mode '(text-mode))
                 (looking-back "^Tasks: *.*? *\\([^,]*\\)"
                               (line-beginning-position))
                 (match-string-no-properties 1)))
    (candidates (company-tasks--candidates arg))
    (annotation (company-tasks--annotation arg))
    (meta (company-tasks--meta arg))
    ))

(defun text-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-reviewers)
  (add-to-list 'company-backends 'company-tasks)
  )

(add-hook 'text-mode-hook 'text-mode-hook-setup)

(provide 'j-completion)
