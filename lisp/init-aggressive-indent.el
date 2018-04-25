(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :config
  ;; (global-aggressive-indent-mode)
  )

(provide 'init-aggressive-indent)
