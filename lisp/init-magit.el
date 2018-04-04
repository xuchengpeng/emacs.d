(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  )

(use-package magithub
  :after magit
  :ensure t
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/.emacs.d/magithub"))

(provide 'init-magit)
