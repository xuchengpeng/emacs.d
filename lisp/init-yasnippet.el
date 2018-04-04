(use-package yasnippet
  :ensure t
  :defer 5
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets :ensure t)
  )

(provide 'init-yasnippet)
