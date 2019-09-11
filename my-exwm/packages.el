;;; packages.el --- my-exwm layer packages file for Spacemacs.
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


;;; Code:

(defconst my-exwm-packages
  '(
    (exwm :step pre)
    (my-exwm-config :location local)
    buffer-expose
    exwm-x
    ;; exwm-edit
    ;; helm-exwm
    ;; evil-exwm-state
    ;; exwm-surf
    ;; exwm-firefox-core
    ;; exwm-firefox-evil
    )
  )

(defun my-exwm/init-exwm ()
  (require 'exwm)
  (require 'exwm-config)
  ;; (exwm-config-default)
  (exwm-enable)
  )

(defun my-exwm/init-my-exwm-config ()
  (use-package my-exwm-config
    :config
    )
  )

(defun my-exwm/init-exwm-x ()
  (use-package exwm-x
    )
  )

;;; buffer alt-tab
(defun my-exwm/init-buffer-expose ()
  (require 'buffer-expose)
  (buffer-expose-mode 1)
  ;; alt-tab,
  ;; s-tab, alt exwm buffer
  ;; (exwm-input-set-key (kbd "s-<tab>") 'buffer-expose)
  ;; 隐藏emacs的buffer，只显示exwm buffer.

  (defun exwm-buffer-list ()
    "return exwm buffer list"
    (mapcar (lambda (x)
              (cdr x))
            exwm--id-buffer-alist)
    )

  (defun exwm-buffer-expose (&optional max)
    "Expose buffers of `buffer-list'.

If MAX is given it determines the maximum number of windows to
show per page, which defaults to `buffer-expose-max-num-windows'."
    (interactive "P")
    (buffer-expose-show-buffers (exwm-buffer-list) max))

  (exwm-input-set-key (kbd "s-<tab>") 'exwm-buffer-expose)
  )
;;; packages.el ends here
