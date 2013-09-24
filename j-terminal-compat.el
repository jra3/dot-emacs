;; Note, we are expecting an xterm compatible terminal here. tmux requires you
;; to set xterm-mode as a terminal option for this
;;
;; quirks
;; iTerm2 does not distiguish between M-ret and M-S-ret by default
(defadvice terminal-init-xterm (after map-S-up-escape-sequence activate)

  (define-key input-decode-map "\e[1;9A" [M-up])
  (define-key input-decode-map "\e[1;9B" [M-down])
  (define-key input-decode-map "\e[1;9C" [M-right])
  (define-key input-decode-map "\e[1;9D" [M-left])

  (define-key input-decode-map "\e[1;10A" [M-S-up])
  (define-key input-decode-map "\e[1;10B" [M-S-down])
  (define-key input-decode-map "\e[1;10C" [M-S-right])
  (define-key input-decode-map "\e[1;10D" [M-S-left])

  ;; weird
  (global-set-key [select] [S-up])
)

(provide 'j-terminal-compat)
