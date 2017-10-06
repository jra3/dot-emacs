;; (require 'org-protocol)

;; An example of how custom sub-protocols work

;; (add-to-list 'org-protocol-protocol-alist
;;              '("Hello World" :protocol "hello-world"
;;                :function my-hello-world
;;             :kill-client t
;;             ))
;; ;; org-protocol:/hello-world:/

;; (defun my-hello-world (data)
;;   "Say hello to the world."
;;   (message data)
;;   (sit-for 3)
;;   nil)

;; org-fast-tag-selection-single-key nil

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(provide 'j-org)
