;;; packages.el --- my-default layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: dingyi <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst my-default-packages
  '(
    (my-const :location local)
    (my-funcs :location local)
    (my-keybinding :location local)
    beacon
    ;; load-relative
    ;; (esup :step bootstrap)
    )
)

(defun my-default/init-my-const ()
  (require 'my-const)
  (global-set-key (kbd "M-E") 'eval-expression)
;;;; disable emacs evil selection auto copy
  (setq x-select-enable-clipboard nil)
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; disable yy dd auto copy
  (setq mouse-drag-copy-region t)
  ;; primary复制使能,禁用，会导致evil的yy,dd复制
  (setq x-select-enable-primary t)
  ;; clipboard复制使能
  (setq x-select-enable-clipboard t)
  ;; 默认是都可以,否则就不能查词了等等
  (spacemacs/set-leader-keys "tY" 'my/toggle-evil-auto-copy)
  )

(defun my-default/init-my-funcs ()
  (use-package my-funcs
    )
  )

(defun my-default/init-my-keybinding ()
  (use-package my-keybinding
    ;; (emacs-init-time) "7.537085889 seconds"
    :init
    (add-hook 'after-init-hook #'my-keybinding-init)
    ;; :config
    ;; (my-keybinding-init)
    )
  )

(defun my-default/init-beacon ()
  (use-package beacon
    :config
    (beacon-mode 1)
    ;; 橘黄色跟spacemacs默认的normal小的光标颜色一样，配。
    (setq beacon-color "Orange" beacon-size 90 beacon-blink-duration 0.4 beacon-blink-delay 0.2)

    ;; (defun my-recenter ()
    ;;   (interactive)
    ;;   (call-interactively 'recenter)
    ;;   (beacon-blink)
    ;;   )
    ;; (general-def "C-l" 'my-recenter)
    ;; (general-def "C-l" 'recenter)
    )
  )

(defun my-default/init-load-relative ()
  (use-package load-relative
    :config

    )
  )

;; (defun my-default/init-esup ()
;;   (use-package esup
;;     )
;;   )

;; ;;; packages.el ends here
