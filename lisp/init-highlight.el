;; init-highlight.el --- Initialize highlighting configurations.
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
;; Highlighting configurations.
;;

;;; Code:

;; Highlight the current line
(add-hook 'after-init-hook #'global-hl-line-mode)

;; Highlight symbols
(use-package highlight-symbol
  :disabled
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :commands (highlight-symbol-mode)
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.5)
  )

;; Highlight symbols
(use-package symbol-overlay
  :ensure t
  :defer t
  :diminish symbol-overlay-mode
  :commands symbol-overlay-mode
  :hook (prog-mode . symbol-overlay-mode)
  )

;; Highlight matching paren
(add-hook 'after-init-hook #'show-paren-mode)
;; (setq show-paren-style 'expression)

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; Colorize color names in buffers
(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  )

;; Highlight uncommitted changes
(use-package diff-hl
  :ensure t
  :defer 3
  :diminish diff-hl-mode
  :config
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(provide 'init-highlight)

;;; init-highlight.el ends here