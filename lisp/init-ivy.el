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
  :diminish
  :hook (after-init . ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume))
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-initial-inputs-alist nil
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-magic-tilde nil
        ivy-dynamic-exhibit-delay-ms 150
        projectile-completion-system 'ivy)
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done))

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
  :diminish
  :hook (after-init . counsel-mode)
  :bind (("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-x r b" . counsel-bookmark)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(provide 'init-ivy)

;;; init-ivy.el ends here
