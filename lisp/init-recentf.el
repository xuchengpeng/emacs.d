(use-package recentf
  :defer 1
  :config
  (setq recentf-save-file (concat dotemacs-cache-directory "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never
        recentf-exclude (list "/tmp/" "/ssh:"
                              (concat user-emacs-directory "elpa/")))
  (recentf-mode +1))

(provide 'init-recentf)
