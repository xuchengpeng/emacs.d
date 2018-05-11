(eval-after-load "abbrev" '(diminish 'abbrev-mode "Abv"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "simple"
                 '(progn
                    (diminish 'auto-fill-function)
                    (diminish 'visual-line-mode)))

(setq user-full-name "Chuck"
      user-mail-address "me@xuchengpeng.com")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun s-font()
  (interactive)
  (set-default-font "Source Code Pro 11")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft YaHei" :size 14)))
  ;; tune rescale so that Chinese character width = 2 * English character width
  ;; (setq face-font-rescale-alist '(("monospace" . 1.0) ("WenQuanYi" . 1.23)))
  )

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (s-font))))

(if window-system
    (s-font))

;; (global-linum-mode t)
(if (version< emacs-version "26")
    (use-package nlinum
      :ensure t
      :config
      (global-nlinum-mode 1))
  (global-display-line-numbers-mode))

;; highlight the current line
(global-hl-line-mode t)

(line-number-mode t)
(column-number-mode t)

(setq scroll-preserve-screen-position 'always)

(show-paren-mode t)
;; (setq show-paren-style 'expression)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil
              tab-width 4)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq display-time-24hr-format t)
(display-time-mode 1)

(defvar dotemacs-cache-directory (concat user-emacs-directory "cache/"))

(setq-default
  make-backup-files nil
  auto-save-default nil
  auto-save-list-file-name (concat dotemacs-cache-directory "autosave")
  backup-directory-alist   (list (cons "." (concat dotemacs-cache-directory "backup/")))
  )

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(provide 'init-default)
