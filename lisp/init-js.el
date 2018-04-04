(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq-default js2-basic-offset 2
                js2-basic-indent 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (setq mode-name "JS2")
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  )

(use-package js2-refactor
  :ensure t
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  )

(provide 'init-js)
