;; init-ivy.el --- Initialize ivy configurations.
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
;; Ivy configurations.
;;

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("TAB" . ivy-next-line)
         ("RET" . ivy-alt-done))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'full
        ivy-magic-tilde nil
        ivy-dynamic-exhibit-delay-ms 150
        ivy-count-format "(%d/%d) "
        projectile-completion-system 'ivy)
  ;; Integration with `magit'
  (dotemacs-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package swiper
  :after ivy
  :diminish
  :bind ("C-s" . swiper))

(use-package counsel
  :after ivy
  :demand t
  :diminish counsel-mode
  :hook (after-init . counsel-mode)
  :bind (("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-x r b" . counsel-bookmark)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable))
  :config
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -M 120 --smart-case --no-heading --line-number --color never %s %s")
          ((executable-find "ag")
           "ag --smart-case --noheading --nocolor --numbers %s %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))
  (setq counsel-rg-base-command "rg -M 120 --smart-case --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag --smart-case --nocolor --nogroup --numbers %s"))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(provide 'init-ivy)

;;; init-ivy.el ends here
