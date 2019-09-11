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

(defconst theme-packages
  '(
    kaolin-themes
    doom-themes
    color-theme-sanityinc-solarized
    nord                                ;这个是浅紫色吗，很好看，有种梦幻的感觉，梦幻紫。
    )
  )

  (defun theme/init-kaolin-themes ()
    (use-package kaolin-themes))

  (defun theme/init-doom-themes ()
    (use-package doom-themes))
;;; packages.el ends here
