(use-package projectile
  :ensure t
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'helm
        projectile-enable-caching t
        projectile-cache-file (concat dotemacs-cache-directory "projectile.cache")
        projectile-known-projects-file (concat dotemacs-cache-directory "projectile-bookmarks.eld")
        )
  (setq-default projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode)
  (helm-projectile-on)
  )

(provide 'init-projectile)
