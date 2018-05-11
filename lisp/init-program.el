;; turn on visual-line-mode
(dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook gfm-mode-hook))
  (add-hook hook 'visual-line-mode))

;; c
(setq-default c-basic-offset 4
              default-tab-width 4
              tab-width 4
              indent-tabs-mode nil)
(setq c-default-style "linux")

;;shell
;; (dolist (exp '("\\.sh\\'" "\\.zsh\\'"
;;                "\\.bash_profile\\'" "\\.bash_history\\'"
;;                "\\.bash\\'" "\\.bashrc.local\\'" "\\.bashrc\\'"))
;;   (add-to-list 'auto-mode-alist
;;                (cons exp 'sh-mode)))
(add-auto-mode 'sh-mode
               "\\.sh\\'" "\\.zsh\\'"
               "\\.bash_profile\\'" "\\.bash_history\\'"
               "\\.bash\\'" "\\.bashrc.local\\'" "\\.bashrc\\'")

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
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-expanding t)
  )

;;js
(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :diminish (js2-mode . "JS2")
  :config
  (setq-default js2-basic-offset 2
                js2-basic-indent 2
                js2-bounce-indent-p nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  )

(use-package js2-refactor
  :ensure t
  :defer t
  :diminish
  :commands (js2-refactor-mode)
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
  :defer t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  )

(provide 'init-program)
