;;; packages.el --- my-dired layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-dired-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-dired/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-dired/pre-init-PACKAGE' and/or
;;   `my-dired/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-dired-packages
  '(
    ;; dired
    dired-hacks-utils                   ;dired-hacks包依赖的共有的函数
    dired-x
    dired-toggle-sudo
    dired-toggle
    dired-filter
    dired-quick-sort
    dired-narrow
    find-by-pinyin-dired
    ;; dired-pinyin                        ;使得很多dired命令都支持拼音搜索
    ;; dired-hydra                         ;hydra支持
    ;; dired-magit-transient               ;magit的操作方式
    (grep-dired :location (recipe :fetcher github
                                  :repo manateelazycat/grep-dired))
    ;; 快速切换dired buffer
    helm-dired-history
    dired-avfs                          ;可以直接查看压缩包
    dired-rainbow
    dired-ranger
    dired-collapse
    dired-open
    ;; fm-bookmarks                        ;在dired上打开dolpfin等的书签
    )
  )

(defun my-dired/init-dired-hacks-utils ()
  (use-package dired-hacks-utils
    :config
    (general-def dired-mode-map
      "<mouse-8>" 'dired-toggle-up-directory
      "<double-mouse-1>" 'dired-open-xdg
      "<down-moues-1>" 'mouse-drag-throw
      "<down-mouse-2>" 'mouse-drag-drag
      )
    (defun dired-mouse-find-file-other-window (event)
      "In Dired, visit the file or directory name you click on."
      (interactive "e")
      (let (window pos file)
        (save-excursion
          (setq window (posn-window (event-end event))
                pos (posn-point (event-end event)))
          (if (not (windowp window))
              (error "No file chosen"))
          (set-buffer (window-buffer window))
          (goto-char pos)
          (setq file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (or (and (cdr dired-subdir-alist)
                     (dired-goto-subdir file))
                (progn
                  (select-window window)
                  (dired-other-window file)))
          (select-window window)
          (find-file-other-window (file-name-sans-versions file t)))))
    (defun dired-mouse-find-file (event)
      "In Dired, visit the file or directory name you click on."
      (interactive "e")
      (let (window pos file)
        (save-excursion
          (setq window (posn-window (event-end event))
                pos (posn-point (event-end event)))
          (if (not (windowp window))
              (error "No file chosen"))
          (set-buffer (window-buffer window))
          (goto-char pos)
          (setq file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (or (and (cdr dired-subdir-alist)
                     (dired-goto-subdir file))
                (progn
                  (select-window window)
                  (dired file)))
          (select-window window)
          (find-file (file-name-sans-versions file t)))))
    (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

    ))

(defun my-dired/post-init-dired-x ()
  (use-package dired-x
    :demand
    :config

    (let ((cmd (cond
                ((eq system-type 'darwin) "open")
                ((eq system-type 'gnu/linux) "xdg-open")
                ((eq system-type 'windows-nt) "start")
                (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd)))
      )
    ;; (general-def dired-mode-map "RET" 'dired-open-guess-shell-alist)

    ;; (setq dired-omit-files
    ;;       (concat dired-omit-files
    ;;               "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))
    )
  )

(defun my-dired/init-dired-toggle-sudo ()
  (use-package dired-toggle-sudo
    :defer (spacemacs/defer)
    :config
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode "S" 'dired-toggle-sudo)
    ))

(defun my-dired/init-dired-toggle ()
  (use-package dired-toggle
    :defer (spacemacs/defer)
    :bind (("<f3>" . #'dired-toggle)
           :map dired-mode-map
           ("q" . #'dired-toggle-quit)
           ([remap dired-find-file] . #'dired-toggle-find-file)
           ([remap dired-up-directory] . #'dired-toggle-up-directory)
           ("C-c C-u" . #'dired-toggle-up-directory))
    :config
    (setq dired-toggle-window-size 32)
    (setq dired-toggle-window-side 'left)

    ;; Optional, enable =visual-line-mode= for our narrow dired buffer:
    (add-hook 'dired-toggle-mode-hook
              (lambda () (interactive)
                (visual-line-mode 1)
                (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
                (setq-local word-wrap nil)))
    )
  )

(defun my-dired/init-dired-filter ()
  "
;; * `dired-filter-by-name' /n
;; * `dired-filter-by-regexp' /r
;; * `dired-filter-by-extension' /h
;; * `dired-filter-by-dot-files'
;; * `dired-filter-by-omit'
;; * `dired-filter-by-garbage'
;; * `dired-filter-by-predicate'
;; * `dired-filter-by-file'
;; * `dired-filter-by-directory'
;; * `dired-filter-by-mode'
;; * `dired-filter-by-symlink' /s
;; * `dired-filter-by-executable'
"
  (use-package dired-filter
    :init
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode
      "f" 'dired-filter-map
      )
    :config
    ;; support chinese pinyin.
    ;; 默认在dired-mode下启用 dired-filter-mode
    (add-hook 'dired-mode-hook #'dired-filter-mode)
    )
  )

(defun my-dired/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :config
    (dired-quick-sort-setup)
    )
  )

(defun my-dired/init-dired-narrow ()
  (use-package init-dired-narrow
    :defer (spacemacs/defer)
    :config
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode
      "s" 'dired-narrow-fuzzy)

    ))

(defun my-dired/post-init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    ))

(defun my-dired/init-grep-dired ()
  (use-package grep-dired
    :defer (spacemacs/defer)
    ))

(defun my-dired/init-helm-dired-history ()
  (use-package helm-dired-history
    :defer (spacemacs/defer)
    :config
    (require 'savehist)
    (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
    (savehist-mode 1)
    ))

;; 需要先安装 avfs,就可以不解压查看压缩包里的文件了
;; pacman -S avfs
;; mountavfs
(defun my-dired/init-dired-avfs ()
    (use-package dired-avfs
      ))
;; 设置默认的打开程序
(defun my-dired/init-dired-open ()
  (use-package dired-open
    ))

;; 设置文件颜色，比如最近几天打开的设置为绿色
(defun my-dired/init-dired-rainbow ()
    (use-package dired-rainbow
      ))

;; 复制粘贴文件，分为两步
;; 书签功能
(defun my-dired/init-dired-ranger ()
    (use-package dired-ranger
      ))

;; 显示子目录下的文件混合
(defun my-dired/init-dired-collapse ()
    (use-package dired-collapse
      ))

(defun my-dired/init-fm-bookmarks ()
  (use-package fm-bookmarks
    :config
    (setq fm-bookmarks-custom-bookmarks
          '(("Root" . "/")
            ("Tmp" . "/tmp/")
            ))
    (setq fm-bookmarks-enable-mounted-media t)
    )
  )

;;; packages.el ends here
