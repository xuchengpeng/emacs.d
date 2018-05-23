;;; init.el --- Chuck's Emacs Configuration

(when (< emacs-major-version 25)
  (error "Emacs should be version 25 or greater"))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq garbage-collection-messages t)
(setq file-name-handler-alist nil
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-private.el"
;;----------------------------------------------------------------------------
(require 'init-preload-private nil t)

(require 'init-utils)
(require 'init-package)
(require 'init-default)
(require 'init-font)
(require 'init-frame)
(require 'init-mode-line)
(require 'init-themes)
(require 'init-filetree)
(require 'init-exec-path)
(require 'init-eshell)
(require 'init-helm)
(require 'init-projectile)
;; (require 'init-pt)
(require 'init-recentf)
(require 'init-dired)
(require 'init-company)
(require 'init-flycheck)
(require 'init-undo-tree)
(require 'init-diff-hl)
(require 'init-which-key)
;; (require 'init-winum)
(require 'init-ace-window)

;; ivy & swiper & counsel
;; (require 'init-ivy)

(require 'init-uniquify)
(require 'init-program)
(require 'init-magit)
(require 'init-multiple-cursors)
;; (require 'init-dashboard)
(require 'init-org)
(require 'init-yasnippet)
(require 'init-smartparens)
(require 'init-hydra)
(require 'init-highlight-symbol)
(require 'init-expand-region)
(require 'init-aggressive-indent)
(require 'init-rainbow-delimiters)
(require 'init-rainbow-mode)
(require 'init-hideshow)

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
