(use-package exec-path-from-shell
  :ensure t
  :defer 5
  :if (memq window-system '(mac ns x))
  :config
  ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize)
  )

(provide 'init-exec-path)
