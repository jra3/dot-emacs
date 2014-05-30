;;============================================================
;; iswitchb
;;============================================================
(require 'iswitchb)
(iswitchb-mode 1)
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;============================================================
;; iswitchb ignores
;;============================================================
(add-to-list 'iswitchb-buffer-ignore "^ ")
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*ECB")
(add-to-list 'iswitchb-buffer-ignore "*IBuffer")
(add-to-list 'iswitchb-buffer-ignore "*Pymacs*")
(add-to-list 'iswitchb-buffer-ignore "*Occur*")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")

;;============================================================
;; iswitchb-fc
;;============================================================
(require 'filecache)
(require 'iswitchb-fc)

(provide 'j-iswitchb)
