(use-package smartparens
  :ensure t
  :defer 6
  :hook (prog-mode . smartparens-strict-mode)
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    )
  )

(provide 'init-smartparens)
