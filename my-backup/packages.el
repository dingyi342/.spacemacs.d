;;; packages.el --- backup layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq my-backup-packages
  '(
    ;; (helm-backup :excluded t)
    ;; backup-walker                       ;在备份中浏览
    ))

(defun my-backup/init-backup-walker ()
  (use-package backup-walker
    :defer (spacemacs/defer)
    :init
    (progn
      (autoload 'backup-walker-start "backup-walker"
        "start walking with the latest backup" t)
      (comma-def :infix "B"
        "w" 'backup-walker-start)
      )
    :config
    ;; 在spacemacs的 spacemacs-default里定义过了，默认是禁用备份，因为我这个加载比较晚所以覆盖了。
    (setq autosave-dir (concat spacemacs-cache-directory "autosaves/")
          auto-save-list-file-prefix (concat spacemacs-cache-directory
                                             "autosaves/auto-save-list/.saves-"))
    (if (not (file-exists-p autosave-dir))
        (make-directory autosave-dir t))
    (add-to-list 'auto-save-file-name-transforms
                 `("\\`/?\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat autosave-dir "\\2") t))
    ;; tramp autosaves
    (setq tramp-auto-save-directory (concat spacemacs-cache-directory "autosaves/tramp-autosaves/"))
    (if (not (file-exists-p tramp-auto-save-directory))
        (make-directory tramp-auto-save-directory))

    (setq make-backup-files t    ;;enable backup files
          vc-make-backup-files t  ;;vcs manage files also backup
          version-control t        ;;带数字编号的备份文件的版本控制
          kept-new-versions 256
          kept-old-versions 0
          delete-old-versions t
          backup-by-copying t)
    ;;备份文件夹的位置，如果不存在新建。
    (setq backup-dir (concat spacemacs-cache-directory "backup/")) ;;设置备份文件夹
    (if (not (file-exists-p backup-dir))
        (make-directory backup-dir))
    (add-to-list 'backup-directory-alist  ;;设置规则
                 `(".*" . ,backup-dir))
    (defun force-backup-of-buffer ()
      (setq buffer-backed-up nil))
    (add-hook 'before-save-hook 'force-backup-of-buffer)
    ;; this is what tramp uses
    (setq tramp-backup-directory-alist backup-directory-alist)
    ))

;;; helm-backup
(defun my-backup/init-helm-backup ()
  (use-package helm-backup
    :config
    (require 'helm-backup)
    (general-def "C-c B" 'helm-backup)
    ))

;;; packages.el ends here
