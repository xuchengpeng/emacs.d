(use-package flycheck
  :ensure t
  :defer 2
  :commands (flycheck-mode global-flycheck-mode)
  :config
  (global-flycheck-mode)
  )

(provide 'init-flycheck)
