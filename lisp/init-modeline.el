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

(defmacro dotemacs-modeline-def-modeline-segment (name &rest forms)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "dotemacs-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst dotemacs--prepare-modeline-segments (segments)
  (cl-loop for seg in segments
           if (stringp seg)
            collect seg
           else
            collect (list (intern (format "dotemacs-modeline-segment--%s" (symbol-name seg))))))

(defmacro dotemacs-modeline-def-modeline (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it. NAME is a symbol to identify
it (used by `dotemacs-modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `dotemacs-modeline-def-modeline-segment'.
Example:
  (dotemacs-modeline-def-modeline minimal
    (bar matches \" \" buffer-info)
    (media-info major-mode))
  (dotemacs-set-modeline 'minimal t)"
  (let ((sym (intern (format "dotemacs-modeline-format--%s" name)))
        (lhs-forms (dotemacs--prepare-modeline-segments lhs))
        (rhs-forms (dotemacs--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun dotemacs-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "dotemacs-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun dotemacs-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let* ((modeline (dotemacs-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))

(use-package eldoc-eval
  :disabled
  :ensure t
  :config
  (defun dotemacs-modeline-eldoc (text)
    (concat (when (display-graphic-p)
              (dotemacs-modeline--make-xpm
               (face-background 'dotemacs-modeline-eldoc-bar nil t)
               dotemacs-modeline-height
               dotemacs-modeline-bar-width))
            text))

  ;; Show eldoc in the mode-line with `eval-expression'
  (defun dotemacs-modeline--show-eldoc (input)
    "Display string STR in the mode-line next to minibuffer."
    (with-current-buffer (eldoc-current-buffer)
      (let* ((str              (and (stringp input) input))
             (mode-line-format (or (and str (or (dotemacs-modeline-eldoc str) str))
                                   mode-line-format))
             mode-line-in-non-selected-windows)
        (force-mode-line-update)
        (sit-for eldoc-show-in-mode-line-delay))))
  (setq eldoc-in-minibuffer-show-fn #'dotemacs-modeline--show-eldoc)

  (eldoc-in-minibuffer-mode +1))

;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
(use-package evil-anzu
  :disabled
  :requires evil
  :init
  (add-transient-hook! #'evil-ex-start-search (require 'evil-anzu))
  (add-transient-hook! #'evil-ex-start-word-search (require 'evil-anzu))
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-minimum-input-length 1
        anzu-search-threshold 250)
  ;; Avoid anzu conflicts across buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state
          anzu--cached-count anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p))
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook '+evil-esc-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status))


;; Keep `dotemacs-modeline-current-window' up-to-date
(defvar dotemacs-modeline-current-window (frame-selected-window))
(defun dotemacs-modeline|set-selected-window (&rest _)
  "Sets `dotemacs-modeline-current-window' appropriately"
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq dotemacs-modeline-current-window win))))

(add-hook 'window-configuration-change-hook #'dotemacs-modeline|set-selected-window)
(add-hook 'focus-in-hook #'dotemacs-modeline|set-selected-window)
(advice-add #'handle-switch-frame :after #'dotemacs-modeline|set-selected-window)
(advice-add #'select-window :after #'dotemacs-modeline|set-selected-window)

;;
;; Variables
;;

(defvar dotemacs-modeline-height 29
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar dotemacs-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)

;;
;; Custom faces
;;

(defgroup dotemacs-modeline nil
  ""
  :group 'dotemacs)

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

(defface dotemacs-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `dotemacs-modeline--anzu', `dotemacs-modeline--evil-substitute' and
`iedit'"
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-success
  `((t (:inherit (success bold))))
  "Face for success messages in the modeline. Used by `*flycheck'."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-error
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group 'dotemacs-modeline)

;; Bar
(defface dotemacs-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group 'dotemacs-modeline)


;;
;; Modeline helpers
;;

(defsubst dotemacs-modeline--active ()
  (eq (selected-window) dotemacs-modeline-current-window))

;; Inspired from `powerline's `pl/make-xpm'.
(defun dotemacs-modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
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
      'xpm t :ascent 'center))))

;;
;; Segments
;;

(dotemacs-modeline-def-modeline-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(dotemacs-modeline-def-modeline-segment buffer-info
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'dotemacs-modeline-buffer-modified)
               ((dotemacs-modeline--active) 'dotemacs-modeline-buffer-file))))

;;
(dotemacs-modeline-def-modeline-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

;;
(dotemacs-modeline-def-modeline-segment major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-major-mode)))

;;
(dotemacs-modeline-def-modeline-segment vcs
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* (backend (vc-backend buffer-file-name))
      (let ((face    'mode-line-inactive)
            (active  (dotemacs-modeline--active)))
        (concat " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))

;;
(dotemacs-modeline-def-modeline-segment flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ((\` not-checked) nil)
      ((\` no-checker) (propertize " -" 'face 'dotemacs-modeline-warning))
      ((\` running) (propertize " ?" 'face 'dotemacs-modeline-success))
      ((\` errored) (propertize " !" 'face 'dotemacs-modeline-error))
      ((\` finished)
       (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
              (no-errors (cdr (assq 'error error-counts)))
              (no-warnings (cdr (assq 'warning error-counts)))
              (face (cond (no-errors 'dotemacs-modeline-error)
                          (no-warnings 'dotemacs-modeline-warning)
                          (t 'dotemacs-modeline-success))))
         (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                     'face face)))
      ((\` interrupted) " -")
      ((\` suspicious) '(propertize " ?" 'face 'dotemacs-modeline-warning))
      )))

;;
(defsubst dotemacs-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(dotemacs-modeline-def-modeline-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (dotemacs-modeline--active) (or mark-active (eq evil-state 'visual)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (eq 'block evil-visual-selection))
                (let ((cols (abs (- (dotemacs-column reg-end)
                                    (dotemacs-column reg-beg)))))
                  (format "%dx%dB" lines cols)))
               ((eq 'line evil-visual-selection)
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL" (- (1+ reg-end) reg-beg) lines))
               (t
                (format "%dC" (- (1+ reg-end) reg-beg)))))
       'face 'dotemacs-modeline-highlight))))


;;
(defsubst dotemacs-modeline--anzu ()
  "Show the match index and total number thereof. Requires `anzu', also
`evil-anzu' if using `evil-mode' for compatibility with `evil-search'."
  (when (and anzu--state (not iedit-mode))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel))))

(defsubst dotemacs-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and evil-mode
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel))))

(defun dotemacs-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst dotemacs-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'dotemacs-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (dotemacs-modeline--active) 'dotemacs-modeline-panel))))

(dotemacs-modeline-def-modeline-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (dotemacs-modeline--anzu)
                      (dotemacs-modeline--evil-substitute)
                      (dotemacs-modeline--iedit))))
     (or (and (not (equal meta "")) meta)
         (if buffer-file-name " %I "))))

;; TODO Include other information
(dotemacs-modeline-def-modeline-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))

(dotemacs-modeline-def-modeline-segment bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (dotemacs-modeline--make-xpm
       (face-background (if (dotemacs-modeline--active)
                            'dotemacs-modeline-bar
                          'dotemacs-modeline-inactive-bar)
                        nil t)
       dotemacs-modeline-height
       dotemacs-modeline-bar-width)
    ""))

;;
;; position
;;

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

(dotemacs-modeline-def-modeline-segment buffer-position
  "The buffer position information."
  '(" " mode-line-position " "))

;;
;; Mode lines
;;

(dotemacs-modeline-def-modeline main
  (bar matches " " buffer-info buffer-position selection-info)
  (buffer-encoding major-mode vcs flycheck))

(dotemacs-modeline-def-modeline minimal
  (bar matches " " buffer-info)
  (media-info major-mode))

(dotemacs-modeline-def-modeline special
  (bar matches " " buffer-info buffer-position selection-info)
  (buffer-encoding major-mode flycheck))

(dotemacs-modeline-def-modeline project
  (bar buffer-default-directory)
  (major-mode))

(dotemacs-modeline-def-modeline media
  (bar " %b  ")
  (media-info major-mode))


;;
;; Hooks
;;

(defun dotemacs-modeline|init ()
  "Set the default modeline."
  (dotemacs-set-modeline 'main t)

  ;; This scratch buffer is already created and doesn't get a modeline. For the
  ;; love of Emacs, someone give the man a modeline!
  (with-current-buffer "*scratch*"
    (dotemacs-set-modeline 'main)))

(defun dotemacs-modeline|set-special-modeline ()
  (dotemacs-set-modeline 'special))

(defun dotemacs-modeline|set-media-modeline ()
  (dotemacs-set-modeline 'media))

(defun dotemacs-modeline|set-project-modeline ()
  (dotemacs-set-modeline 'project))


;;
;; Bootstrap
;;

(add-hook 'after-init-hook #'dotemacs-modeline|init)
;; (add-hook 'dotemacs-scratch-buffer-hook #'dotemacs-modeline|set-special-modeline)
;; (add-hook 'dotemacs-dashboard-mode-hook #'dotemacs-modeline|set-project-modeline)

(add-hook 'image-mode-hook   #'dotemacs-modeline|set-media-modeline)
(add-hook 'org-src-mode-hook #'dotemacs-modeline|set-special-modeline)
(add-hook 'circe-mode-hook   #'dotemacs-modeline|set-special-modeline)

(provide 'init-modeline)

;;; init-modeline.el ends here
