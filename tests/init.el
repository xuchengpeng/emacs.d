;;; init.el --- Chuck's Emacs Configuration.
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
;; Chuck's Emacs Configuration.
;;

;;; Code:

(when (< emacs-major-version 25)
  (error "Emacs should be version 25 or greater"))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(setq garbage-collection-messages t)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)
           ))

(setq debug-on-error t)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-private.el"
;;----------------------------------------------------------------------------
(require 'init-preload-private nil t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
       )
)

(require 'init-package)
(require 'init-basic)
(require 'init-ui)
(require 'init-edit)
(require 'init-utils)
(require 'init-helm)
;; ivy & swiper & counsel
;; (require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-highlight)
(require 'init-window)

(require 'init-eshell)
(require 'init-projectile)
(require 'init-flycheck)
(require 'init-magit)

;; (require 'init-org)
(require 'init-program)

(require 'server)
(unless (server-running-p)
  (server-start))

(when (file-exists-p custom-file)
  (load custom-file))

;;--------------------------------------------------------------------------------------------
;; Allow users to provide an optional "init-afterload-private.el" containing personal settings
;;--------------------------------------------------------------------------------------------
(require 'init-afterload-private nil t)

(provide 'init)

;;; init.el ends here
