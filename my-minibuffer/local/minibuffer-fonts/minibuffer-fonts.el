;;; minibuffer-fonts.el --- minibuffer fonts config  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <dingyi@dingyi>
;; Keywords: 

(defun minibuffer-fonts-set ()
  )


;; (defun my/revert-minibuffer-cjk-fonts ()
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font
;;      (frame-parameter nil 'font)
;;      charset
;;      (font-spec :name "-GOOG-Sarasa Gothic J-bold-italic-normal-*-*-*-*-*-*-0-iso10646-1"
;;                 :weight 'normal
;;                 :slant 'normal
;;                 :size 19.5))nil)
;;   )


;; (add-hook 'minibuffer-setup-hook 'my/set-minibuffer-cjk-fonts)
;; (add-hook 'minibuffer-exit-hook 'my/revert-minibuffer-cjk-fonts)


;; (defun my-minibuffer-setup ()
;;   (set (make-local-variable 'face-remapping-alist)
;;        '((default :height 1))))

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)

;; ;; (defface my-minibuffer-face
;;   '((t (:family "Source Code Pro" :weight normal :height 16.0)
;;        ))
;;   "Face for my minibuffer"
;;   :group 'my-minibuffer-face
;;   )

; Any available font name
;; (setq my-font-name "DejaVu Sans Mono")
;; (defcustom my-font-size 12 "My font size")

;; (defun set-frame-font-size (&optional font-size)
;;   "Change frame font size to FONT-SIZE.
;; If no FONT-SIZE provided, reset the size to its default variable."
;;   (let ((font-size
;;      (or font-size
;;        (car (get 'my-font-size 'standard-value)))))
;;     (customize-set-variable 'my-font-size font-size)
;;     (set-frame-font
;;      (format "%s %d" my-font-name font-size) nil t)))

;; (defun increase-frame-font ()
;;   "Increase frame font by one."
;;   (interactive)
;;   (set-frame-font-size (+ my-font-size 1)))

;; (defun decrease-frame-font ()
;;   "Decrease frame font by one."
;;   (interactive)
;;   (set-frame-font-size (- my-font-size 1)))

;; (defun reset-frame-font ()
;;   "Reset frame font to its default value."
;;   (interactive)
;;   (set-frame-font-size))


(provide 'minibuffer-fonts)
