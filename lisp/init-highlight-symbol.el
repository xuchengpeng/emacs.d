(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :commands (highlight-symbol-mode)
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.5)
  )

(provide 'init-highlight-symbol)
