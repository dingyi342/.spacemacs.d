;;; packages.el --- theme layer packages file for Spacemacs.
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

;; doom-dracula                   ;可以，不过粉太刺眼了
;; doom-nord  ;可以
;; doom-nord-light
;; nord ;这个是浅紫色吗，很好看，有种梦幻的感觉，梦幻紫。

;;; Code:

(defconst my-themes-packages
  '(
    kaolin-themes
    doom-themes
    color-theme-sanityinc-solarized
    nord-theme                                ;这个是浅紫色吗，很好看，有种梦幻的感觉，梦幻紫。
    atom-one-dark-theme
    flucui-themes                       ; 还不错 drak/light 都还不错
    lab-themes
    srcery-theme                        ; 也还行吧,但是 git diff 我还是喜欢红绿色.
    ;; gruvbox-theme
    ;; zenburn
    ;; dracula
    ;; per-buffer-theme
    )
  )

(defun my-themes/init-kaolin-themes ()
  (use-package kaolin-themes))

(defun my-themes/init-doom-themes ()
  (use-package doom-themes))

(defun my-themes/init-nord-theme ()
  (use-package nord-theme))

(defun my-themes/init-color-theme-sanityinc-solarized ()
  (use-package color-theme-sanityinc-solarized
    )
  )

(defun my-themes/init-atom-one-dark-theme ()
  (use-package atom-one-dark-theme))

(defun my-themes/init-flucui-themes ()
  (use-package flucui-themes))

(defun my-themes/init-lab-themes ()
  (use-package lab-themes))

(defun my-themes/init-per-buffer-theme ()
  (use-package per-buffer-theme
    :config
    )
  )

;;; packages.el ends here
