(use-package winum
  :disabled
  :ensure t
  :bind (("M-0" . winum-select-window-0-or-10)
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9))
  :config  
  (require 'winum)
  
  (defun winum-assign-9-to-calculator-8-to-flycheck-errors ()
    (cond
     ((equal (buffer-name) "*Calculator*") 9)
     ((equal (buffer-name) "*Flycheck errors*") 8)))
  
  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 10))
  
  (add-to-list 'winum-assign-functions #'winum-assign-9-to-calculator-8-to-flycheck-errors)
  (add-to-list 'winum-assign-functions #'winum-assign-0-to-neotree)
  
  (set-face-attribute 'winum-face nil :weight 'bold)
  
  (setq window-numbering-scope            'global
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
        winum-assign-func                 'my-winum-assign-func
        winum-auto-setup-mode-line        t
        winum-mode-line-position          1
        winum-ignored-buffers             '(" *which-key*"))
  
  (winum-mode)
  )

(provide 'init-winum)
