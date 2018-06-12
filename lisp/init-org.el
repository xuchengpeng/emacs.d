(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode)
         ("\\.txt$" . org-mode))
  :bind ("C-c a" . org-agenda)
  :config
  (defvar dotemacs-org-directory (concat user-emacs-directory "org/"))
  (setq org-agenda-files (list (concat dotemacs-org-directory "work.org")
                               (concat dotemacs-org-directory "gtd.org")))
  (setq org-todo-keywords
      '((sequence "TODO" "WAITING" "SOMEDAY" "|" "DONE" "CANCELLED")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE")))
  )

(provide 'init-org)
