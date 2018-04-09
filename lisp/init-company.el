(use-package company
  :ensure t
  :defer 5
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 15
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil)
  (global-company-mode 1)
  )

(provide 'init-company)
