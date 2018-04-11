(use-package projectile
  :ensure t
  :defer 5
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (progn
    (setq projectile-completion-system 'helm
          projectile-enable-caching t
          projectile-cache-file (concat dotemacs-cache-directory "projectile.cache")
          projectile-known-projects-file (concat dotemacs-cache-directory "projectile-bookmarks.eld")
          )
    (projectile-global-mode)
    (helm-projectile-on))
  )

(provide 'init-projectile)
