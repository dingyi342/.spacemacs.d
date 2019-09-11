;;; packages.el --- my-autosave layer packages file for Spacemacs.
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
;; 参见这里的讨论
;; https://emacs-china.org/t/topic/7687
;; 不过我发现我不是怎么喜欢auto-save.经常会删除但并不小保存的情况存在。
;; 本来缓冲区和实际存储的文件的隔离被auto-save分不清了。
;; 我觉得可以了我在手动保存一下，最后由git diff来提交等，
;; 分阶段暂存，这样，再一起提交。
;;; 适合我的自动保存姿势
;; 1. 不使用所有 真自动保存方法
;; 2. 手动保存，绑定到M-s
;; 3. emacs的 auto save，防止特殊情况的崩溃,spacemacs自动开启了。并且再次打开会提示恢复。
;; 4. 用git做迭代。
;; 5. 虽然用真自动保存，但是与很多东西冲突，要解决，其次如果是配置文件有可能没注意就保存了
;; 导致系统出问题，找不到。保存前调用ediff.
;;; 开岂emacs自带的 auto-save.

;;; Code:
(defconst my-autosave-packages
  '(
    (auto-save :location (recipe :fetcher github
                                 :repo "manateelazycat/auto-save"))
    ;; (focus-autosave-mode :excluded t)   ;; frame失去焦点自动保存。
    ;; real-auto-save         ;; 每隔多少秒就自动保存。
    ;; super-save                           ; save Emacs buffers when they lose focus
    )
  )

(defun my-autosave/init-auto-save ()
  ;; auto-save-visited-mode
    (require 'auto-save)
    (setq auto-save-silent t)
    (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
    (auto-save-enable)
  )

(defun my-autosave/init-real-auto-save ()
  (use-package real-auto-save
    :config
    (setq real-auto-save-interval 7) ;; in seconds
    (add-hook 'org-mode-hook 'real-auto-save-mode)
    (add-hook 'prog-mode-hook 'real-auto-save-mode)
    )
  )

(defun my-autosave/init-focus-autosave-mode ()
  (use-package focus-autosave
    :config
    (focuse-autosave-mode)              ;global
    ;; (focus-autosave-local-mode)         ;buffer-loal
    )
  )

(defun my-autosave/init-super-save ()
  (use-package super-save
    :config
    (super-save-mode +1)
    ;; idle闲置的时候自动保存
    (setq super-save-auto-save-when-idle 5)
    ;; 设置idle闲置时间，默认是5，因为已经有很多自动保存的触发方法了，所以设置5还好。
    ;; (setq super-save-idle-duration 3)
    (setq auto-save-default nil)        ;关闭内置的自动保存
    ;; 默认会在 command(switch-to-buffer)，hook triggers(focus-out-hook)上触发保存文件
    ;; 可以增加命令和钩子
    (add-to-list 'super-save-triggers 'ace-window)
    (add-to-list 'super-save-triggers 'ace-window)
    (add-to-list 'super-save-hook-triggers 'find-file-hook)
    ;; 编辑完了以后,bury buffer还是蛮常用的。
    ;; (add-to-list 'super-save-triggers 'bury-buffer)
    ;; 关闭对远程文件的自动保存，因为会很卡
    (setq super-save-remote-files nil)
    ;; 自动保存所有buffer，默认只对当前buffer生效。
    ;; https://github.com/bbatsov/super-save/pull/20/files
    (defun my-super-save/save-all-buffers ()
      (save-excursion
        (dolist (buf (buffer-list))
          (set-buffer buf)
          (when (and buffer-file-name
                     (buffer-modified-p (current-buffer))
                     (file-writable-p buffer-file-name)
                     (if (file-remote-p buffer-file-name) super-save-remote-files t))
            (save-buffer)))))

    (advice-add 'super-save-command :override 'my-super-save/save-all-buffers)
    ))
;;; packages.el ends here
