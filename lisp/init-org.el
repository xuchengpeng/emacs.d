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
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE")))
  (setq org-log-done 'time
        org-startup-indented t
        org-startup-folded nil)
  )

(provide 'init-org)
