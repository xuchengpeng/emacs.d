;; init-default.el --- Initialize default configurations.
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
;; Default configurations.
;;

;;; Code:

(setq user-full-name "Chuck"
      user-mail-address "me@xuchengpeng.com")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; (global-linum-mode t)
(if (version< emacs-version "26")
    (use-package nlinum
      :ensure t
      :init (add-hook 'after-init-hook #'global-nlinum-mode)
      )
  (add-hook 'after-init-hook #'global-display-line-numbers-mode))

(line-number-mode t)
(column-number-mode t)

(setq scroll-preserve-screen-position 'always)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil
              tab-width 4)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)

(defvar dotemacs-cache-directory (concat user-emacs-directory ".cache/"))

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

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(provide 'init-default)

;;; init-default.el ends here
