(use-package eshell
  :ensure t
  :defer t
  :commands (eshell)
  :config
  (setq
       eshell-buffer-shorthand t
       eshell-history-size 1000
       eshell-save-history-on-exit t
       eshell-hist-ignoredups t
       eshell-cmpl-ignore-case t
       eshell-error-if-no-glob t
       eshell-glob-case-insensitive t
       eshell-scroll-to-bottom-on-input 'all
       eshell-directory-name (concat user-emacs-directory "tmp/eshell")
       eshell-aliases-file (concat user-emacs-directory "eshell/alias")
       )
  )

(provide 'init-eshell)
