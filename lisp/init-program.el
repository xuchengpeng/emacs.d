;; text-mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; c
(setq-default c-basic-offset 4
              default-tab-width 4
              tab-width 4
              indent-tabs-mode nil)
(setq c-default-style "linux")

;;shell
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

;; web
(use-package web-mode
  :ensure t
  :defer t
  :commands (web-mode)
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))

  :config  
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-expanding t)
  )

;;js
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :diminish (js2-mode . "JS2")
  :config
  (setq-default js2-basic-offset 2
                js2-basic-indent 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  )

(use-package js2-refactor
  :ensure t
  :diminish
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  )

;; toml
(use-package toml-mode
  :ensure t
  :defer t
  :mode ("\\.toml$" . toml-mode)
  )

;; yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  )

(provide 'init-program)
