(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this))
  :config
  (setq mc/list-file "~/.emacs.d/tmp/.mc-lists.el")
  )

(provide 'init-multiple-cursors)
