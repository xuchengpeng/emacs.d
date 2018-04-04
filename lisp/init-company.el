(use-package company
  :ensure t
  :defer 5
  :init
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 15)
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode 1)
  )

(provide 'init-company)
