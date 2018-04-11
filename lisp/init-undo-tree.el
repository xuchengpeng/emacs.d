(use-package undo-tree
  :ensure t
  :defer 3
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-history-directory-alist `(("." . ,(concat dotemacs-cache-directory "undo")))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)
    (global-undo-tree-mode)
    )
  )

(provide 'init-undo-tree)
