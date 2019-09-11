;;; packages.el --- my-stumpwm layer packages file for Spacemacs.
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

(defconst my-stumpwm-packages
  '(
    (my-stumpwm :location local)
    stumpwm-mode
    )
  )

(defun my-stumpwm/init-my-stumpwm ()
  (use-package my-stumpwm
    :config
    ;; stumpwm定以的s-v竟然在minibuffe中不起作用
    (general-def minibuffer-local-map "s-v" 'yank)
    )
  )

(defun my-stumpwm/init-stumpwm-mode ()
  "可以执行stumpwm lisp"
  (use-package stumpwm-mode
    :config
    ;; stumpwm-eval-defun
;; stumpwm-eval-region
;; stumpwm-mode
;; stumpwm-eval-last-sexp
;; helm-stumpwm-commands
;; helm-info-stumpwm

    ;; 替代 commonlisp中的一些 major-mode keybinding.
    (spacemacs/set-leader-keys-for-minor-mode 'stumpwm-mode
      "er" 'stumpwm-eval-region
      "ef" 'stumpwm-eval-defun
      "ee" 'stumpwm-eval-last-sexp)
    ))

;;; packages.el ends here
