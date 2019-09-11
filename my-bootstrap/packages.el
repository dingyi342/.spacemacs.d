;;; packages.el --- my-bootstrap layer packages file for Spacemacs.
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

(defconst my-bootstrap-packages
  '(
    ;; bootstrap packages,
    ;; `use-package' cannot be used for bootstrap packages configuration
    (general :step bootstrap)
    (use-package-ensure-system-package :step bootstrap)
    ;; (benchmark-init :step bootstrap)
    (gcmh :step bootstrap)
    )
  )

(defun my-bootstrap/init-general ()
  (require 'general)
  (general-evil-setup)
  (general-evil-setup t)
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace
                                  evilified
                                  ;; exwm
                                  ))

  (general-create-definer comma-def
    :states '(normal evilified)
    ;; :states '(normal evilified motion exwm)
    ;; :keymaps 'override
    :prefix ","
    )

  (general-create-definer semicolon-def
    :states '(normal evilified motion exwm)
    :keymaps 'override
    :prefix ";")

  (general-create-definer spc-def
    :states '(normal evilified motion exwm)
    :keymaps 'override
    :prefix "SPC"
    )

  (general-create-definer sspc-def
    :states '(normal evilified motion emacs)
    :keymaps 'override
    :prefix "s-SPC"
    )

  ;; (general-create-definer exwm-def
  ;;   :states
  ;;   )
  )

(defun my-bootstrap/init-use-package-ensure-system-package ()
  (with-eval-after-load 'use-package
    (with-eval-after-load 'system-packages
      (require 'use-package-ensure-system-package)
      )
    )
  )

;; (defun my-bootstrap/init-benchmark-init ()
;;   (require 'benchmark-init)
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate)
;;   )

(defun my-bootstrap/init-gcmh ()
  (require 'gcmh)
  (gcmh-mode 1)
  ;; (setq gcmh-high-cons-thre)
  (setq
   gcmh-verbose t
   gcmh-idle-delay 15
   gcmh-idle-timer nil
   ;; gcmh-low-cons-threshold 100000000
   ;; gcmh-low-cons-threshold 800000
   gcmh-low-cons-threshold 16777216     ;16mb
   gcmh-high-cons-threshold 1073741824  ;1Gb
   )
  ;; gcmh-mode-hook
  )

;;; packages.el ends here
