;;; packages.el --- my-pinyin layer packages file for Spacemacs.
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
;; 让ivy/helm支持拼音搜索，全拼，拼音首字母，双拼等。
;; 不支持繁体？

;;; Code:

(defconst my-pinyin-packages
  '(
    evil-find-char-pinyin
    ace-pinyin
    ;; pinyin
    ;; pinyinlib
    ;; find-by-pinyin-dired
    pinyin-search
    (iswitchb-pinyin  :location local)
    pyim                                ;里用pyim的库让ivy支持拼音搜索
    )
  )

(defun my-pinyin/init-evil-find-char-pinyin ()
  "f/F can search chinese with pinyin."
  (use-package evil-find-char-pinyin
    :config
    (evil-find-char-pinyin-mode +1)))

(defun my-pinyin/init-ace-pinyin ()
  "jump to Chinese characters usign avy or ace-jump-mode"
  (use-package ace-pinyin
    :config
    ;; By default this package is using avy.
    ;; use ace-jump-mode instead of avy:
    ;; set before you call `ace-pinyin-global-mode'
    ;; (setq ace-pinyin-use-avy nil)

    ;; globally enable ace-pinyin.
    (ace-pinyin-global-mode +1)

    ;; supported all avy commands.

    ;; By default, only supports 简体, set to support 繁体。
    ;; (setq ace-pinyin-simplified-chinese-only-p nil)
    ))

;; https://emacs-china.org/t/helm/8737/4
(defun my-pinyin/init-iswitchb-pinyin ()
 ;; (setq pinyin-initials-file "~/.spacemacs.d/my-pinyin/local/iswitchb-pinyin/pinyin-initials.txt")
 (require 'iswitchb-pinyin)
 ;; 支持中文拼音首字母匹配，会使helm-find-files匹配过多。
 (cl-defun helm-mm-3-match/around (orig-fn str &rest args)
   (apply orig-fn (concat str "|" (str-unicode-to-pinyin-initial str)) args))
 (advice-add 'helm-mm-3-match :around #'helm-mm-3-match/around)

 ;; 默认在输入前面加空格解决匹配问题,这个在 find-file里也加空格就很烦了。
 ;; 但是如果在find-file不加空格就匹配不到了，需要时再手打空格吧。
 ;; 只支持拼音首字母，不支持全拼
 ;; (defun helm-find-files-1/around (orig-fn fname &rest args)
 ;;   (apply orig-fn (concat fname " ") args))
 ;; (advice-add 'helm-find-files-1 :around #'helm-find-files-1/around)
 )

(defun my-pinyin/post-init-pyim ()
  (use-package pyim
    :after ivy
    :config
;;; 使得ivy支持 全拼 拼音首字母
    (defun eh-ivy-cregexp (str)
      (let ((a (ivy--regex-plus str))
            (b (let ((case-fold-search nil))
                 (pyim-cregexp-build str))))
        (if (and a (stringp a))
            (concat a "\\|" b)
          a)))

    (setq ivy-re-builders-alist
          '((t . eh-ivy-cregexp)))
    )
  )

(defun my-pinyin/init-pinyin-search ()
  (use-package pinyin-search
    :config
    )
  )

;;; packages.el ends here
