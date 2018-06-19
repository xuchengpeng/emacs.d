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

(setq display-time-24hr-format t)
(add-hook 'after-init-hook #'display-time-mode)

(provide 'init-ui)
