;; init-ui.el --- Initialize UI configurations.
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
;; UI configurations.
;;

;;; Code:

;; maximized startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default frame-title-format '("%f - " user-full-name))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'text-mode)

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

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  )

(use-package base16-theme
  :disabled
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t))

(use-package dracula-theme
  :disabled
  :ensure t
  :config
  (load-theme 'dracula t)
  )

;; modeline configurations
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'arrow)
  (powerline-default-theme)
  )

(use-package spaceline
  :disabled
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )

(use-package smart-mode-line
  :disabled
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup)
  )

;; font configurations
;; Solution 1
(defun dotemacs-font-existsp (font)
  "Detect if a font exists"
  (if (null (x-list-fonts font))
      nil
    t))
;; or
;; (defun dotemacs-font-existsp (font)
;;   "Detect if a font exists"
;;   (if (find-font (font-spec :family font))
;;         t
;;       nil))

(defun dotemacs-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun dotemacs-set-font (english-fonts
                          english-font-size
                          chinese-fonts
                          &optional chinese-font-size chinese-fonts-scale)
  (setq chinese-font-size (or chinese-font-size 16)
        chinese-fonts-scale (or chinese-fonts-scale 1.2))

  "english-font-size could be set to \":pixelsize=18\" or a integer.
   If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl) ; for find if
  (let ((en-font (dotemacs-make-font-string
                  (find-if #'dotemacs-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'dotemacs-font-existsp chinese-fonts)
                            :size chinese-font-size)))

    ;; Set English font
    ;; (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font)))
    
    ;; Fix chinese font width and rescale
    (setq face-font-rescale-alist '(("STHeiti" . ,chinese-fonts-scale)
                                    ("STFangsong" . ,chinese-fonts-scale)
                                    ("Microsoft Yahei" . ,chinese-fonts-scale)
                                    ("WenQuanYi Micro Hei Mono" . ,chinese-fonts-scale))))

(defun set-font()
  (interactive)
  (dotemacs-set-font
    '("DejaVu Sans Mono" "Monaco" "Source Code Pro" "Consolas") ":pixelsize=14"
    '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体") 16)
  )

;; Solution 2
;; (defun set-font()
;;   (interactive)
;;   (setq fonts
;;         (cond ((eq system-type 'darwin)     '("Monaco"           "STHeiti"))
;;               ((eq system-type 'gnu/linux)  '("Menlo"            "WenQuanYi Zen Hei"))
;;               ((eq system-type 'windows-nt) '("DejaVu Sans Mono" "Microsoft Yahei"))))
;;   (set-face-attribute 'default nil :font
;;                       (format "%s:pixelsize=%d" (car fonts) 14))
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font) charset
;;                       (font-spec :family (car (cdr fonts)) :size 16)))
;;   ;; Fix chinese font width and rescale
;;   (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2)))
;;   )

;; Solution 3
;; (defun set-font()
;;   (interactive)
;;   
;;   ;; Setting English Font
;;   (when (member "DejaVu Sans Mono" (font-family-list))
;;     (set-face-attribute 'default nil :font
;;                         (format "%s:pixelsize=%d" "DejaVu Sans Mono" 14))
;;     )
;;   
;;   ;; Setting Chinese font
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset
;;                       (font-spec :family "Microsoft Yahei" :size 16))
;;     )
;;   
;;   ;; Fix chinese font width and rescale
;;   (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2)))
;;   )

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (set-font))))

(if window-system
    (set-font))

(provide 'init-ui)

;;; init-ui.el ends here
