(use-package solarized-theme
  :ensure t
  :defer t
  :config
  (load-theme 'solarized-dark t)
  )

(use-package zenburn-theme
  :ensure t
  :defer t
  :config
  (load-theme 'zenburn t)
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  )

(use-package base16-theme
  :ensure t
  :defer t
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package dracula-theme
  :ensure t
  :defer t
  :config
  (load-theme 'dracula t)
  )

(provide 'init-themes)
