# dotemacs-themes

```el
(require 'dotemacs-themes)

;; Global settings (defaults)
(setq dotemacs-themes-enable-bold t    ; if nil, bold is universally disabled
      dotemacs-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (dotemacs-one, dotemacs-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme theme dotemacs-one)

;; Enable flashing mode-line on errors
(dotemacs-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(dotemacs-themes-org-config)
```
