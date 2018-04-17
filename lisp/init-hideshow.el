(use-package hideshow
  :diminish hs-minor-mode
  :commands (hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode)
  :bind (:map prog-mode-map
              ("C-c h" . hs-toggle-hiding))
  )

(provide 'init-hideshow)
