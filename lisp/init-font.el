;; Solution 1
;; (defun dotemacs-font-existsp (font)
;;   (if (null (x-list-fonts font))
;;       nil
;;     t))
;; 
;; (defun dotemacs-make-font-string (font-name font-size)
;;   (if (and (stringp font-size)
;;            (equal ":" (string (elt font-size 0))))
;;       (format "%s%s" font-name font-size)
;;     (format "%s %s" font-name font-size)))
;; 
;; (defun dotemacs-set-font (english-fonts
;;                        english-font-size
;;                        chinese-fonts
;;                        &optional chinese-font-size)
;; 
;;   "english-font-size could be set to \":pixelsize=18\" or a integer.
;;    If set/leave chinese-font-size to nil, it will follow english-font-size"
;;   (require 'cl) ; for find if
;;   (let ((en-font (dotemacs-make-font-string
;;                   (find-if #'dotemacs-font-existsp english-fonts)
;;                   english-font-size))
;;         (zh-font (font-spec :family (find-if #'dotemacs-font-existsp chinese-fonts)
;;                             :size chinese-font-size)))
;; 
;;     ;; Set the default English font
;;     (message "Set English Font to %s" en-font)
;;     (set-face-attribute 'default nil :font en-font)
;; 
;;     ;; Set Chinese font
;;     (message "Set Chinese Font to %s" zh-font)
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset zh-font))))
;; 
;; (dotemacs-set-font
;;   '("DejaVu Sans Mono" "Monaco" "Source Code Pro" "Consolas") ":pixelsize=14"
;;   '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体") 16)

;; Solution 2
;; (setq fonts
;;       (cond ((eq system-type 'darwin)     '("Monaco"           "STHeiti"))
;;             ((eq system-type 'gnu/linux)  '("Menlo"            "WenQuanYi Zen Hei"))
;;             ((eq system-type 'windows-nt) '("DejaVu Sans Mono" "Microsoft Yahei"))))
;; (set-face-attribute 'default nil :font
;;                     (format "%s:pixelsize=%d" (car fonts) 14))
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset
;;                     (font-spec :family (car (cdr fonts)) :size 16)))
;; ;; Fix chinese font width and rescale
;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))

;; Solution 3
(defun set-font()
  (interactive)
  
  ;; Setting English Font
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" "DejaVu Sans Mono" 14))
  
  ;; Setting Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "Microsoft Yahei" :size 16))
    )
  
  ;; Fix chinese font width and rescale
  (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2)))
  )

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (set-font))))

(if window-system
    (set-font))

(provide 'init-font)
