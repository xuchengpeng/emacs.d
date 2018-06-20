;; init-utils.el --- Initialize ultilities.
;;
;; Copyright (C) 2018 xuchengpeng
;;
;; Author: xuchengpeng <xucp@outlook.com>
;; URL: https://github.com/xuchengpeng/emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some useful Utilities.
;;

;;; Code:

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  )

;; filetree configurations
(use-package neotree
  :disabled
  :ensure t
  :bind ("<f8>" . neotree-toggle)
  )

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :bind ("<f8>" . treemacs)
  :config
  (setq treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name "treemacs-persist" dotemacs-cache-directory)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)
  
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  )

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package pt
  :disabled
  :ensure t
  :commands (pt-regexp projectile-pt)
  )

(provide 'init-utils)

;;; init-utils.el ends here
