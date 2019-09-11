;;; packages.el --- my-system-packages layer packages file for Spacemacs.
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

;; 可以用emacs来管理package
;; 可以配何 use-package-ensure-system-package 来自动检测安装系统的包
;; 现在只在arch测试。

;;; Code:

(defconst my-system-packages-packages
  '(
    (system-packages :step bootstrap)
    helm-system-packages
    helm-systemd
    )
)

(defun my-system-packages/init-system-packages ()
  (use-package system-packages
    :config
    (require 'system-packages)
    ;; 不能设为t , 不能使用sudo,提示找不到sudo
    (setq system-packages-use-sudo nil)
    (setq system-packages-package-manager 'yay)
    (add-to-list 'system-packages-supported-package-managers
             '(yay .
                      ((default-sudo . nil)
                       (install . "yay -S")
                       (search . "yay -Ss")
                       (uninstall . "yay -Rs")
                       (update . "yay -Syu")
                       (clean-cache . "yay -Sc")
                       (log . "cat /var/log/pacman.log")
                       (get-info . "yay -Qi")
                       (get-info-remote . "yay -Si")
                       (list-files-provided-by . "yay -Ql")
                       (verify-all-packages . "yay -Qkk")
                       (verify-all-dependencies . "yay -Dk")
                       (remove-orphaned . "yay -Rns $(pacman -Qtdq)")
                       (list-installed-packages . "yay -Qe")
                       (list-installed-packages-all . "yay -Q")
                       (list-dependencies-of . "yay -Qi")
                       (noconfirm . "--noconfirm"))))
    )
  )

(defun my-system-packages/init-helm-system-packages ()
  (use-package helm-system-packages
    :commands (helm-system-packages)
    ;; 需要 pacman -S expac 用来解析 pacman 数据库的
    :general
    (spc-def "Sp" 'helm-system-packages)
    ))

(defun my-system-packages/init-helm-systemd ()
  (use-package helm-systemd
    :commands (helm-systemd)
    :general
    (spc-def "Ss" 'helm-systemd)
    ))

;;; packages.el ends here
