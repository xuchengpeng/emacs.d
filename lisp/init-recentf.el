(use-package recentf
  :defer 10
  :config
  (setq recentf-save-file (concat dotemacs-cache-directory "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 600
        recentf-exclude (list "/tmp/" "/ssh:"
                              (concat user-emacs-directory "elpa/")))
  (recentf-mode +1))

(provide 'init-recentf)
