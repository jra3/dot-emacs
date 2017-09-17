(require 'erc)
(require 'tls)


;; This causes ERC to connect to the Freenode network upon hitting
;; C-c e f.  Replace MYNICK with your IRC nick.
(global-set-key "\C-cef" (lambda () (interactive)
                           (erc-tls :server "irc.tfbnw.net"
                                    :port "6443"
                                    :nick "jallen")))

(defun erc-switch-to-buffer ()
  "read events(chars), and switch to appropriate erc buffer"
  (interactive)
  (let ((buffers (erc-channel-list nil))
        buffer
        (index))
    ;;  lookup-key
    (while (let ((char (char-to-string (read-event "channel #: ")))
                 )
             (setq buffer
                   (cond ((string-match "[a-z]" char)
                          (aget '(("e" . "#e")
                                  ("p" . "#perf")
                                  ("t" . "#tupperware")
                                  )
                                char 't))
                         ((string-match "[0-9]" char)
                          (nth (string-to-int char) buffers)))))
      (switch-to-buffer buffer))))

(define-key erc-mode-map (kbd "M-g") 'erc-switch-to-buffer)

(provide 'j-irc)