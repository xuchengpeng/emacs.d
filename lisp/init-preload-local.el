(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

(setq load-prefer-newer t)

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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/tmp/backup")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/tmp/autosave" t)))

(setq make-backup-files nil  ; stop creating backup~ files
      auto-save-default nil) ; stop creating #autosave# files

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(provide 'init-preload-local)
