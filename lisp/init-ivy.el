(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(provide 'init-ivy)
