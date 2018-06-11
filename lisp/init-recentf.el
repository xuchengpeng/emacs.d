(use-package recentf
  :defer 2
  :config
  (setq recentf-save-file (concat dotemacs-cache-directory "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 600
        recentf-exclude '("/tmp/" "/ssh:" "/elpa/"))
  (recentf-mode +1))

(provide 'init-recentf)
