;;; packages.el --- my-archlinux layer packages file for Spacemacs.
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
;; 1. 写archlinux依赖的包，提示安装
;;; Code:

(defconst my-archlinux-packages
  '(
    (my-archlinux :location local)
    )
)

(defun my-archlinux/init-my-archlinux ()
  (use-package my-archlinux
    :ensure-system-package (
                            zsh
                            vim
                            vlc
                            )
    :config
    )
  )

;;; packages.el ends here
