;;; packages.el --- my-download layer packages file for Spacemacs.
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
;;;; 快捷键，一键添加url
;; 在stumpwm中，设置快捷键，从minibuffer中读取url,添加到aria的下载列表。

;;;; emacs风格的界面已经提供了。
;; 下载器的管理也提供了。

;;;;  支持启用代理。

;;;; 通知
;;;; 查看下载进度
;;;; 直接打开下载
;; 就是在已完成的项目上，按o键，就读取下载写入的地址，根据文件格式，选择相应的程序打开。




;;; Code:

(defconst my-download-packages
  '(
    (aria2 :location (recipe :fetcher github
                             :repo "dingyi342/aria2"))
    )
)

(defun my-download/init-aria2 ()
  (use-package aria2
    :config
    (setq aria2-download-directory "/mnt/e/03videos/aria2/")
    ))

;;; packages.el ends here
