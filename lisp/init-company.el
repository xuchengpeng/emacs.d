(use-package company
  :ensure t
  :defer 5
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 15
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil)
  
  ;; (global-set-key (kbd "M-/") 'company-yasnippet)
  
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  
  (global-company-mode 1)
  )

(provide 'init-company)
