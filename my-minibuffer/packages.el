;;; packages.el --- my-minibuffer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-minibuffer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-minibuffer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-minibuffer/pre-init-PACKAGE' and/or
;;   `my-minibuffer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-minibuffer-packages
  '(
    ;; minibuffer-cua
    ;; minibuf-isearch
    ;; minibuffer-complete-cycle
    ;; minibuffer-line
    (minibuffer-fonts :location local)
    )
)

(defun my-minibuffer/init-minibuffer-fonts ()
  (use-package minibuffer-fonts
    :config
    ;; (setq
    ;;  ;; minibuffer-setup-hook
    ;;  ;; minibuffer-frame-alist
    ;;  ;; minibuffer-scroll-window
    ;;  ;; minibuffer-completing-symbol
    ;;  ;; minibuffer-completion-confirm
    ;;  ;; max-mini-window-height
    ;;  ;; ivy-fixed-height-minibuffer
    ;;  ;; max-mini-window-height
    ;;  ;; minibuffer-line-mode
    ;;  )

    (setq minibuffer-message-timeout 0.5)
    (setq minibuffer-auto-raise nil)
    (setq resize-mini-frames t)

    ;; 当把modeline隐藏了，就可以设置为grow-only.了否则会顶着modeline晃来晃去
    (setq resize-mini-windows nil)
    ;; (setq resize-mini-windows 'grow-only)
    ))

;; (setq font-name "Hack" font-size 9)

;; (defun set-frame-font-size (&optional font-size)
;;   "Sets font size for all frames. Default is font-size"
;;   (interactive (list
;;                 (read-number "number: " font-size)))
;;   (let ((font-name "Hack")
;;         (font-size (or font-size font-size)))
;;     (set-frame-font (format "%s %d" font-name font-size) nil t)))

;; (set-frame-font-size)

;; (defun my-minibuffer-setup ()
;;   (set (make-local-variable 'face-remapping-alist)
;;        '((default :height 0.5))))

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)




;;; packages.el ends here
