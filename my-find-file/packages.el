;;; packages.el --- my-find-file layer packages file for Spacemacs.
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
;; find-file,更像实现了一个文件管理器了。
;; helm已经实现的差不多了，有些小细节和键位绑定修改一下。
;;;; helm-ff 的候选项添加常用位置，比如音乐/书籍/视频等
;; 共选择，相当于一个书签，然后用户输入第一个tab后，就不在添加这些位置。
;;;; helm-ff中使用yasnippet
;; 比如输入 md M-/ 就能补全成 ~/Musics/
;; 也不能调用 M-x
;;;; 利用hydra等快速选择常用地址
;; 就好像 org-ageda一样。
;;; Code:

(defconst my-find-file-packages
  '()
)


;;; packages.el ends here
