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

(defun dotemacs-modeline-project-root ()
  "Get the path to the root of your project.
If STRICT-P, return nil if no project was found, otherwise return
`default-directory'."
  (let (projectile-require-project-root)
    (projectile-project-root)))

(use-package all-the-icons
  :ensure t)

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

;; fish-style modeline
(use-package shrink-path
  :ensure t
  :commands (shrink-path-prompt shrink-path-file-mixed))


;;
;; Variables
;;

(defvar dotemacs-modeline-height 29
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar dotemacs-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar dotemacs-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defvar dotemacs-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `dotemacs-modeline-buffer-file-name'.

Given ~/Projects/FOSS/emacs/lisp/comint.el
truncate-upto-project => ~/P/F/emacs/lisp/comint.el
truncate-upto-root => ~/P/F/e/lisp/comint.el
truncate-all => ~/P/F/e/l/comint.el
relative-from-project => emacs/lisp/comint.el
relative-to-project => lisp/comint.el
file-name => comint.el")

;; externs
(defvar anzu--state nil)
(defvar evil-mode nil)
(defvar evil-state nil)
(defvar evil-visual-selection nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


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

(defface dotemacs-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group 'dotemacs-modeline)

(defface dotemacs-modeline-urgent
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

(defun dotemacs-modeline-maybe-icon-octicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-octicon args)))

(defun dotemacs-modeline-maybe-icon-faicon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-faicon args)))

(defun dotemacs-modeline-maybe-icon-material (&rest args)
  "Display material icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-material args)))

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

(defun dotemacs-modeline-buffer-file-name ()
  "Propertized `buffer-file-name' based on `dotemacs-modeline-buffer-file-name-style'."
  (propertize
   (pcase dotemacs-modeline-buffer-file-name-style
     ('truncate-upto-project (dotemacs-modeline--buffer-file-name 'shrink))
     ('truncate-upto-root (dotemacs-modeline--buffer-file-name-truncate))
     ('truncate-all (dotemacs-modeline--buffer-file-name-truncate t))
     ('relative-to-project (dotemacs-modeline--buffer-file-name-relative))
     ('relative-from-project (dotemacs-modeline--buffer-file-name-relative 'include-project))
     ('file-name (propertize (file-name-nondirectory buffer-file-name)
                             'face
                             (let ((face (or (and (buffer-modified-p)
                                                  'dotemacs-modeline-buffer-modified)
                                             (and (dotemacs-modeline--active)
                                                  'dotemacs-modeline-buffer-file))))
                               (when face `(:inherit ,face))))))
   'help-echo buffer-file-truename))

(defun dotemacs-modeline--buffer-file-name-truncate (&optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename)))
        (active (dotemacs-modeline--active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'dotemacs-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'dotemacs-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'dotemacs-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'dotemacs-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory buffer-file-name)
                              'face (if file-faces `(:inherit ,file-faces)))))))))

(defun dotemacs-modeline--buffer-file-name-relative (&optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (dotemacs-modeline-project-root))
        (active (dotemacs-modeline--active)))
    (if (null root)
        (propertize "%b" 'face (if active 'dotemacs-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'dotemacs-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory buffer-file-truename)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'dotemacs-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'dotemacs-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory buffer-file-truename)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun dotemacs-modeline--buffer-file-name (truncate-project-root-parent)
  "Propertized `buffer-file-name'.
If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
fish-shell style.

Example:
~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (dotemacs-modeline-project-root))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-truename)
                                                  buffer-file-truename))
         (active (dotemacs-modeline--active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'dotemacs-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,filename) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'dotemacs-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'dotemacs-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'dotemacs-modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize (if truncate-project-root-parent
                                      root-path-parent
                                    (abbreviate-file-name project-root))
                                  'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize filename 'face file-props)))))))))


;;
;; Segments
;;

(dotemacs-modeline-def-modeline-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (dotemacs-modeline--active) 'dotemacs-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (dotemacs-modeline-maybe-icon-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(dotemacs-modeline-def-modeline-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (dotemacs-modeline-maybe-icon-octicon
                          "lock"
                          :face 'dotemacs-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (dotemacs-modeline-maybe-icon-faicon
                          "floppy-o"
                          :face 'dotemacs-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (dotemacs-modeline-maybe-icon-octicon
                          "circle-slash"
                          :face 'dotemacs-modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (dotemacs-modeline-maybe-icon-octicon
                          "fold"
                          :face 'dotemacs-modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (dotemacs-modeline-buffer-file-name)
            "%b")))

;;
(dotemacs-modeline-def-modeline-segment buffer-info-simple
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
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (dotemacs-modeline--active))
            (all-the-icons-default-adjust -0.1))
        (concat (when (eq system-type 'gnu/linux)
                "  "
                (cond ((memq state '(edited added))
                       (if active (setq face 'dotemacs-modeline-info))
                       (dotemacs-modeline-maybe-icon-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face 'dotemacs-modeline-info))
                       (dotemacs-modeline-maybe-icon-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face 'dotemacs-modeline-warning))
                       (dotemacs-modeline-maybe-icon-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face 'dotemacs-modeline-urgent))
                       (dotemacs-modeline-maybe-icon-octicon "alert" :face face))
                      (t
                       (if active (setq face 'font-lock-doc-face))
                       (dotemacs-modeline-maybe-icon-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))))
                " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))

;;
(defun dotemacs-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (dotemacs-modeline-maybe-icon-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text dotemacs-modeline-vspc)))
          (when text
            (propertize text 'face face))
          (if vc-mode "  " " ")))

(dotemacs-modeline-def-modeline-segment flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (dotemacs-ml-icon "do_not_disturb_alt"
                                        (number-to-string sum)
                                        (if .error 'dotemacs-modeline-urgent 'dotemacs-modeline-warning)
                                        -0.25)))
                   (dotemacs-ml-icon "check" nil 'dotemacs-modeline-info)))
      ('running     (dotemacs-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
      ('no-checker  (dotemacs-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
      ('errored     (dotemacs-ml-icon "sim_card_alert" "Error" 'dotemacs-modeline-urgent))
      ('interrupted (dotemacs-ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))
      ;; ('interrupted (dotemacs-ml-icon "x" "Interrupted" 'font-lock-doc-face)))))

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
(defun dotemacs-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (dotemacs-modeline--active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'dotemacs-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'dotemacs-modeline-panel)
              sep
              (dotemacs-modeline-maybe-icon-octicon "triangle-right"
                                     :face 'dotemacs-modeline-panel
                                     :v-adjust -0.05)
              sep))))

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
  (let ((meta (concat (dotemacs-modeline--macro-recording)
                      (dotemacs-modeline--anzu)
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
;; Mode lines
;;

(dotemacs-modeline-def-modeline main
  (bar matches " " buffer-info-simple "  %l:%c %p  " selection-info)
  (buffer-encoding major-mode vcs flycheck))

(dotemacs-modeline-def-modeline minimal
  (bar matches " " buffer-info-simple)
  (media-info major-mode))

(dotemacs-modeline-def-modeline special
  (bar matches " " buffer-info-simple "  %l:%c %p  " selection-info)
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
