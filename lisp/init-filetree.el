(use-package neotree
  :disabled
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  )

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs)
  :bind ("<f8>" . treemacs)
  :config
  (setq treemacs-follow-after-init t
        treemacs--persist-file (concat dotemacs-cache-directory "treemacs-persist"))
  
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  )

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t
  :defer t)

(provide 'init-filetree)
