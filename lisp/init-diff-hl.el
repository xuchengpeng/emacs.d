(use-package diff-hl
  :ensure t
  :defer 6
  :diminish diff-hl-mode
  :config
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(provide 'init-diff-hl)
