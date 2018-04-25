(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'arrow)
  (powerline-default-theme)
  )

(use-package spaceline
  :disabled
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )

(use-package smart-mode-line
  :disabled
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup)
  )

(provide 'init-mode-line)
