;;; packages.el --- my-eshell layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-eshell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-eshell/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-eshell/pre-init-PACKAGE' and/or
;;   `my-eshell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-eshell-packages
  '(
    eshell
    )
)

(defun my-eshell/post-init-eshell ()
  (use-package eshell
    :config
    (setq eshell-buffer-name "Eshell")
    ;; 包装 eshell-command 从minibuffer输入命令执行。就类似 shell-command.
    ;; 比shell-command好的是，还可以执行elisp等，比如 d . 打开dired.
    (defun eshell-command-from-minibuffer ()
      (interactive)
      (eshell-command (read-from-minibuffer "eshell-command: "))
      )
    :general
    (semicolon-def "e" 'eshell-command-from-minibuffer)
    ))

;;; packages.el ends here
