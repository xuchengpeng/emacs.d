(use-package hydra
  :ensure t
  :defer t
  :bind (("C-x t" . hydra-toggle/body)
         ("C-M-o" . hydra-window/body))
  :config
  (defhydra hydra-toggle (:color pink
                          :hint nil)
    "
_a_: abbrev-mode
_d_: debug-on-error
_f_: auto-fill-mode
_o_: org-mode
_t_: truncate-lines
_w_: whitespace-mode
_q_: Quit

    "
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("o" org-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil)
    )
  (defhydra hydra-window (:color pink
                          :hint nil)
    "
               -- WINDOW MENU --
^^^^^^^^-----------------------------------------------------
^Move Point^     ^Move Splitter^  ^Split^
^^^^^^^^-----------------------------------------------------
_<up>_           _<S-up>_         _0_: delete-window
_<left>_         _<S-left>_       _1_: delete-other-windows
_<down>_         _<S-down>_       _2_: split-window-below
_<right>_        _<S-right>_      _3_: split-window-right
You can use arrow-keys or WASD.
_q_: Quit
    "
    ("0" delete-window :exit t)
    ("1" delete-other-windows :exit t)
    ("2" split-window-below :exit t)
    ("3" split-window-right :exit t)
    ("a" windmove-left nil)
    ("s" windmove-down nil)
    ("w" windmove-up nil)
    ("d" windmove-right nil)
    ("A" hydra-move-splitter-left nil)
    ("S" hydra-move-splitter-down nil)
    ("W" hydra-move-splitter-up nil)
    ("D" hydra-move-splitter-right nil)
    ("<left>" windmove-left nil)
    ("<down>" windmove-down nil)
    ("<up>" windmove-up nil)
    ("<right>" windmove-right nil)
    ("<S-left>" hydra-move-splitter-left nil)
    ("<S-down>" hydra-move-splitter-down nil)
    ("<S-up>" hydra-move-splitter-up nil)
    ("<S-right>" hydra-move-splitter-right nil)
    ("u" hydra--universal-argument nil)
    ("q" nil)
    )
  )

(provide 'init-hydra)
