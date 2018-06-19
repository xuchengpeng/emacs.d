;; init-edit.el --- Initialize editing configurations.
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
;; Editing configurations.
;;

;;; Code:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default major-mode 'text-mode)

(setq-default
  make-backup-files nil
  auto-save-default nil
  auto-save-list-file-name (concat dotemacs-cache-directory "autosave")
  backup-directory-alist   (list (cons "." (concat dotemacs-cache-directory "backup/")))
  )

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(after-load 'abbrev
  (diminish 'abbrev-mode "Abv"))
(after-load 'eldoc
  (diminish 'eldoc-mode))
(after-load 'autorevert
  (diminish 'auto-revert-mode))
(after-load 'simple
              (diminish 'auto-fill-function)
              (diminish 'visual-line-mode))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook (prog-mode . aggressive-indent-mode)
  :config
  ;; (global-aggressive-indent-mode)
  )

;; Increase selected region by semantic units
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  )

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this))
  :config
  (setq mc/list-file (concat dotemacs-cache-directory ".mc-lists.el"))
  )

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :defer 1
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-history-directory-alist `(("." . ,(concat dotemacs-cache-directory "undo")))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)
    (global-undo-tree-mode)
    )
  )

;; Hideshow
(use-package hideshow
  :diminish hs-minor-mode
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding))
  )

(provide 'init-edit)

;;; init-edit.el ends here
