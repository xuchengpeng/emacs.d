(use-package yasnippet
  :ensure t
  :defer 10
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets :ensure t)
  )

(provide 'init-yasnippet)
