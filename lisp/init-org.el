(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org$" . org-mode)
  :bind ("C-c a" . org-agenda)
  :config
  (defvar dotemacs-org-directory (concat user-emacs-directory "org/"))
  (setq org-agenda-files (list (concat dotemacs-org-directory "work.org")
                               (concat dotemacs-org-directory "gtd.org")))
  )

(provide 'init-org)
