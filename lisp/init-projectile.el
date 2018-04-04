(use-package projectile
  :ensure t
  :defer 5
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-enable-caching t)
    (projectile-global-mode)
    (helm-projectile-on))
  )

(provide 'init-projectile)
