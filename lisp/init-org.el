(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :hook (org-mode . turn-on-auto-fill)
  )

(provide 'init-org)
