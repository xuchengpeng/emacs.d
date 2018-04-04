(use-package recentf
  :defer 1
  :config
  (defconst savefile-dir (expand-file-name "tmp/recentf" user-emacs-directory))
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(provide 'init-recentf)
