;; init-modeline.el --- Initialize modeline.
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
;; Modeline configurations.
;;

;;; Code:


;;
;; Variables
;;

(defgroup dotemacs-modeline nil
  "TODO"
  :group 'faces)

(defface dotemacs-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-success
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc' and `*flycheck'."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*vc' and `*flycheck'"
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-error
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*vc' and `*flycheck'"
  :group 'dotemacs-modeline)

;; Bar
(defface dotemacs-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group 'dotemacs-modeline)


(defvar dotemacs-modeline-height 25
  "How tall the mode-line should be (only respected in GUI Emacs).")

(defvar dotemacs-modeline-buffer-name-function
  #'dotemacs-modeline--file-path
  "TODO.")

(defvar-local mode-line-format-left  ())
(defvar-local mode-line-format-right ())
(put 'mode-line-format-left  'risky-local-variable t)
(put 'mode-line-format-right 'risky-local-variable t)

;;
;; Helpers
;;

(defun dotemacs-modeline-project-root ()
  "Get the path to the root of your project.

If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))

;; Keep `dotemacs-modeline-current-window' up-to-date
(defvar dotemacs-modeline-current-window (frame-selected-window))
(defun dotemacs-modeline|set-selected-window (&rest _)
  "Set `dotemacs-modeline-current-window' appropriately."
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq dotemacs-modeline-current-window win)
      (force-mode-line-update))))

(defun dotemacs-modeline|unset-selected-window ()
  "Set `dotemacs-modeline-current-window' nil."
  (setq dotemacs-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'dotemacs-modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'dotemacs-modeline|set-selected-window)
(advice-add #'select-window :after #'dotemacs-modeline|set-selected-window)
(if (not (boundp 'after-focus-change-function))
    (progn
      (add-hook 'focus-in-hook  #'dotemacs-modeline|set-selected-window)
      (add-hook 'focus-out-hook #'dotemacs-modeline|unset-selected-window))
  (defun dotemacs-modeline|refresh-frame ()
    (setq dotemacs-modeline-current-window nil)
    (cl-loop for frame in (frame-list)
             if (eq (frame-focus-state frame) t)
             return (setq dotemacs-modeline-current-window (frame-selected-window frame)))
    (force-mode-line-update t))
  (add-function :after after-focus-change-function #'dotemacs-modeline|refresh-frame))

(defsubst dotemacs-modeline--active ()
  "Check if modelne is active."
  (eq (selected-window) dotemacs-modeline-current-window))

;; xpm generator
(defun dotemacs-modeline--make-xpm (width height &optional color)
  "Create an XPM bitmap.  Inspired by `powerline''s `pl/make-xpm' with WIDTH, HEIGHT and COLOR."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun dotemacs-modeline--file-path (&optional path)
  "PATH."
  (let ((buffer-file-name (or path buffer-file-name))
        (root (dotemacs-modeline-project-root))
        (active (dotemacs-modeline--active)))
    (cond ((null root)
           (propertize "%b" 'face (if active 'dotemacs-modeline-buffer-file)))
          ((or (null buffer-file-name)
               (directory-name-p buffer-file-name))
           (propertize (abbreviate-file-name (or buffer-file-name default-directory))
                       'face (if active 'dotemacs-modeline-buffer-path)))
          ((let* ((modified-faces (if (buffer-modified-p) 'dotemacs-modeline-buffer-modified))
                  (true-filename (file-truename buffer-file-name))
                  (relative-dirs (file-relative-name (file-name-directory true-filename)
                                                     (concat root "../")))
                  (relative-faces (or modified-faces (if active 'dotemacs-modeline-buffer-path)))
                  (file-faces (or modified-faces (if active 'dotemacs-modeline-buffer-file))))
             (if (equal "./" relative-dirs) (setq relative-dirs ""))
             (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                     (propertize (file-name-nondirectory true-filename)
                                 'face (if file-faces `(:inherit ,file-faces)))))))))


;;
;; Macros
;;

(cl-defmacro dotemacs-modeline-def-modeline-segment (name &rest rest &key init faces hooks vars &allow-other-keys)
  "TODO"
  (let ((body rest))
    (while (keywordp (car body))
      (setq body (cddr body)))
    (setq rest body))
  (unless EMACS26+
    (setq vars nil))
  (let ((docstring (if (stringp (car rest)) (pop rest)))
        (realvar (if (and rest faces) (intern (format "dotemacs-modeline--var-%s" name)) name)))
    (macroexp-progn
     (append (when rest
               (if (or hooks vars)
                   (let ((setterfn    (intern (format "dotemacs-modeline--set-%s" name)))
                         (varsetterfn (intern (format "dotemacs-modeline--setvar-%s" name))))
                     (append `((fset ',setterfn
                                     (lambda (&rest _)
                                       (when (or (memq ',name mode-line-format-left)
                                                 (memq ',name mode-line-format-right))
                                         (setq-local ,realvar (progn ,@rest))))))
                             (mapcar (lambda (hook) `(add-hook ',hook #',setterfn))
                                     hooks)
                             (when vars
                               `((fset ',varsetterfn
                                       (lambda (sym val op where)
                                         (and (eq op 'set) where
                                              (with-current-buffer where
                                                (set sym val)
                                                (,setterfn)))))
                                 ,@(mapcar (lambda (var) `(add-variable-watcher ',var #',varsetterfn))
                                           vars)))))
                 (setq init `(quote (:eval ,@rest)))
                 nil))
             (if (eq realvar name)
                 `((defvar-local ,name ,init ,docstring))
               `((defvar-local ,realvar nil)
                 (defvar-local ,name
                   '(:eval (cond ((dotemacs-modeline--active) ,realvar) (,realvar (substring-no-properties ,realvar))))
                   ,docstring)))
             `((put ',name 'risky-local-variable t))))))

;;
(defvar dotemacs-mode-line-alist nil)

(defun dotemacs-modeline-def-modeline (name left &optional right)
  "Defines a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `dotemacs-modeline' for retrieval).
LEFT and RIGHT are lists of symbols of modeline segments defined with
`dotemacs-modeline-def-modeline-segment'.
Example:
  (dotemacs-modeline-def-modeline minimal
    (bar \" \" buffer-info)
    (media-info major-mode))
  (dotemacs-modeline-def-modeline :project
    '(dotemacs-modeline-bar dotemacs-modeline-buffer-directory)
    '(dotemacs-modeline-major-mode))
  (dotemacs-modeline-set-modeline 'project t)"
  (setf (alist-get name dotemacs-mode-line-alist) (list left right)))

(defun dotemacs-modeline-set-modeline (name &optional default)
  "Set the modeline format.  Does nothing if the modeline NAME doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (let ((modeline (cdr (assq name dotemacs-mode-line-alist))))
    (when modeline
      (if default
          (setq-default mode-line-format-left  `("" ,@(car  modeline))
                        mode-line-format-right `("" ,@(cadr modeline)))
        (setq mode-line-format-left  `("" ,@(car  modeline))
              mode-line-format-right `("" ,@(cadr modeline))))
      (force-mode-line-update))))


;;
;; Bars
;;

(defvar dotemacs-mode-line-bar-active nil "TODO.")
(defvar dotemacs-mode-line-bar-inactive nil "TODO.")
(defun dotemacs-modeline|setup-bars ()
  "Setup bars."
  (setq dotemacs-mode-line-bar-active
        (dotemacs-modeline--make-xpm 3 dotemacs-modeline-height (face-background 'dotemacs-modeline-bar))
        dotemacs-mode-line-bar-inactive
        (dotemacs-modeline--make-xpm 3 dotemacs-modeline-height)))
(add-hook 'dotemacs-load-modeline-hook #'dotemacs-modeline|setup-bars)

(defun dotemacs-modeline|setup-bars-after-change (_sym val op _where)
  "Setup bars after change with VAL(height) and OP(set)."
  (when (eq op 'set)
    (let ((dotemacs-modeline-height val))
      (dotemacs-modeline|setup-bars))))
(add-variable-watcher 'dotemacs-modeline-height #'dotemacs-modeline|setup-bars-after-change)

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-bar
  (if (dotemacs-modeline--active) dotemacs-mode-line-bar-active dotemacs-mode-line-bar-inactive))


;;
;; Segments
;;

(defun dotemacs-modeline|update-on-change ()
  "TODO."
  (dotemacs-modeline--set-dotemacs-modeline-buffer-id)
  (remove-hook 'post-command-hook #'dotemacs-modeline|update-on-change t))
(defun dotemacs-modeline|start-update-on-change ()
  "TODO."
  (add-hook 'post-command-hook #'dotemacs-modeline|update-on-change nil t))
(add-hook 'first-change-hook #'dotemacs-modeline|start-update-on-change)

(advice-add #'undo :after #'dotemacs-modeline--set-dotemacs-modeline-buffer-id)
(advice-add #'undo-tree-undo :after #'dotemacs-modeline--set-dotemacs-modeline-buffer-id)

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-buffer-id
  :hooks (find-file-hook after-change-functions read-only-mode-hook after-save-hook after-revert-hook)
  :faces t
  (concat " "
          (if buffer-file-name
              (funcall dotemacs-modeline-buffer-name-function buffer-file-name)
            "%b")))

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-buffer-directory
  (let ((face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-vcs
  :vars (vc-mode)
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (dotemacs-modeline--active)))
        (cond ((memq state '(edited added))
                (if active (setq face 'dotemacs-modeline-success)))
               ((eq state 'needs-merge)
                (if active (setq face 'dotemacs-modeline-success)))
               ((eq state 'needs-update)
                (if active (setq face 'dotemacs-modeline-warning)))
               ((memq state '(removed conflict unregistered))
                (if active (setq face 'dotemacs-modeline-error)))
               (t
                (if active (setq face 'font-lock-doc-face))))
        (concat " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-encoding
  :hooks (after-save-hook find-file-hook)
  :vars (buffer-file-coding-system)
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                "UTF-8"
              (upcase (symbol-name (plist-get sys :name)))))
          "  "))

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-major-mode
  :vars (mode-name)
  :faces t
  (propertize (format-mode-line mode-name) 'face 'font-lock-keyword-face))

;;
(dotemacs-modeline-def-modeline-segment dotemacs-modeline-flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      (`finished
       (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
              (no-errors (cdr (assq 'error error-counts)))
              (no-warnings (cdr (assq 'warning error-counts)))
              (face (cond (no-errors 'dotemacs-modeline-error)
                          (no-warnings 'dotemacs-modeline-warning)
                          (t 'dotemacs-modeline-success))))
         (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                     'face (if (dotemacs-modeline--active) face))))
      (`interrupted " -")
      (`suspicious '(propertize " ?" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-warning)))
      (`running (propertize " ?" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-success)))
      (`errored (propertize " !" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-error)))
      (`no-checker (propertize " -" 'face (if (dotemacs-modeline--active) 'dotemacs-modeline-warning)))
      ;; (`not-checked nil)
      )))

;;
(defsubst dotemacs-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local dotemacs-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(defun dotemacs-modeline|enable-word-count ()
  "Enable word count."
  (setq dotemacs-modeline-enable-word-count t))
(add-hook 'text-mode-hook #'dotemacs-modeline|enable-word-count)

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-selection-info
  (when mark-active
    (cl-destructuring-bind (beg . end)
        (cons (region-beginning) (region-end))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((bound-and-true-p rectangle-mark-mode)
                        (let ((cols (abs (- (dotemacs-modeline-column end)
                                            (dotemacs-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when dotemacs-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'dotemacs-modeline-highlight))))

;; Be compatible with Emacs 25.
(defvar-local dotemacs-modeline-column-zero-based
  (or (bound-and-true-p column-number-indicator-zero-based) t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar-local dotemacs-modeline-percent-position
  (or (bound-and-true-p mode-line-percent-position) '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(setq-default mode-line-position
              '((line-number-mode
                 (column-number-mode
                  (dotemacs-modeline-column-zero-based " %l:%c" " %l:%C")
                  " %l")
                 (column-number-mode (dotemacs-modeline-column-zero-based " :%c" " :%C")))
                (if dotemacs-modeline-percent-position (" " dotemacs-modeline-percent-position))
                (:eval (when (or line-number-mode column-number-mode dotemacs-modeline-percent-position) " "))))

(dotemacs-modeline-def-modeline-segment dotemacs-modeline-buffer-position
  "The buffer position information."
  '(" " mode-line-position " "))

;;
(dotemacs-modeline-def-modeline-segment dotemacs-modeline-misc-info
  (propertize (format-time-string " %H:%M ")
              'help-echo (format-time-string "%c")))

;;
;; Mode lines
;;

(dotemacs-modeline-def-modeline :main
  '(dotemacs-modeline-bar " " dotemacs-modeline-buffer-id dotemacs-modeline-buffer-position dotemacs-modeline-selection-info)
  '(dotemacs-modeline-encoding dotemacs-modeline-major-mode dotemacs-modeline-vcs dotemacs-modeline-flycheck))

(dotemacs-modeline-def-modeline :project
  '(dotemacs-modeline-bar dotemacs-modeline-buffer-directory)
  '(dotemacs-modeline-major-mode))


;;
;;
;;

(dotemacs-modeline-def-modeline-segment mode-line-rest
  (let ((rhs-str (format-mode-line mode-line-format-right)))
    (list (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(1+ (string-width rhs-str))))))
          rhs-str)))

(setq-default mode-line-format '("" mode-line-format-left mode-line-rest))


;;
(dotemacs-modeline-set-modeline :main t)

;; (add-hook 'dotemacs-dashboard-mode-hook (dotemacs-modeline-set-modeline :project))


;;
(defun dotemacs-modeline-init ()
  "Init modeline."
  (run-hooks 'dotemacs-load-modeline-hook))
(add-hook 'after-init-hook #'dotemacs-modeline-init)

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar dotemacs-modeline-remap-face-cookie nil)
(defun dotemacs-modeline|focus ()
  "Modeline foucus."
  (when dotemacs-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative dotemacs-modeline-remap-face-cookie)))
(defun dotemacs-modeline|unfocus ()
  "Modeline unfocus."
  (setq dotemacs-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))
(add-hook 'focus-in-hook #'dotemacs-modeline|focus)
(add-hook 'focus-out-hook #'dotemacs-modeline|unfocus)


(provide 'init-modeline)

;;; init-modeline.el ends here
