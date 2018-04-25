(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :hook (org-mode . turn-on-visual-line-mode)
  )

(provide 'init-org)
