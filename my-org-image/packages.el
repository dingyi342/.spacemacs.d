;;; packages.el --- my-org-image layer packages file for Spacemacs.
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
;https://stackoverflow.com/questions/15407485/inline-pdf-images-in-org-mode
;https://emacs-china.org/t/topic/5355/28
;;; Code:

(defconst my-org-image-packages
  '(
    org-download
    )
  )

(defun my-org-image/post-init-org-download ()
  (use-package org-download
    :config
    ;; (add-hook 'org-mode-hook #'org-download-enable)
    (progn
      ;; 图片位置
      (setq-default org-download-image-dir (concat my-org-directory ".org_imgs/"))
      ;; 这样就不会在新建名字问headline的文件夹了
      ;; 用setq-default,因为是local-buffer
      (setq-default org-download-heading-lvl nil)

      ;; 图片命名, screenshot_时间戳
      (setq org-download-timestamp "_%Y-%m-%d_%H-%M-%S")

      ;; 下载后端
      ;; (setq org-download-backend "curl")

      ;; 额外的属性
      (setq org-download-image-org-width 500)
      ;; (setq org-download-image-latex-width 500)
      ;; (setq org-download-image-html-width 500)
      ;; (setq org-download-image-attr-list nil)

      ;; 截图方法
      (setq org-download-screenshot-method "deepin-screenshot -s %s")
      ;; (setq org-download-screenshot-method "scrot -s %s")

      ;; 不要在org文档里添加图片注释
      ;; 不能这样设置
      ;; (setq org-download-annotate-function 'ignore)
      (setq org-download-annotate-function (lambda (_link) ""))
      ;; 顺便改下annotate，就是自动插入的那行注释，里面写的是图片来源路径
      ;; (setq org-download-annotate-function
      ;;       '(lambda (link)
      ;;          (org-download-annotate-default (org-link-unescape link))))

      ;; org-下载方法
      ;; (defun custom-org-download-method (link)
      ;;   (org-download--fullname (org-link-unescape link)))
      ;; (setq org-download-method 'custom-org-download-method) ; 注意：这里不能用lambda表达式
      )
    )
  )

;;; packages.el ends here
