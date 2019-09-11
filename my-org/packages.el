;;; packages.el --- my-org layer packages file for Spacemacs.
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

;;; Code:

(defconst my-org-packages
  '(
    org
    find-file-in-project
    deft
    helm-org-rifle
    ;; (org-ql :location (recipe :fetcher github
    ;;                           :repo "alphapapa/org-ql"))

    )
  )

(defun my-org/post-init-org ()
  (use-package org
    :config
    (setq org-src-window-setup
          'split-window-below
          ;; 'other-window
          ;; 'current-window
          )
    (setq org-src-tab-acts-natively nil)  ;spacemacs设置的为t,这样会导致 yasnippet tab占位符出现问题

    ;; org bullets
    (setq org-bullets-bullet-list '("♠" "♣" "♥" "♦" "♤" "♧" "♡" "♢"))

    ;; open with
    (add-to-list 'org-file-apps '("\\.mp4\\'" . "vlc %s"))

    ;;org list不自动添加空行
    ;;@https://emacs.stackexchange.com/questions/22109/how-can-i-prevent-empty-lines-between-lists-from-causing-org-mode-to-insert-blan
    (setcdr (assoc 'plain-list-item org-blank-before-new-entry) nil)

    ;; 使用 yas-indent-line的auto方式，并不能正确缩进org-mode中的java代码
    (add-hook 'org-mode-hook '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

    (defun helm-projectile-do-search-in-org-directory ()
      (interactive)
      (spacemacs/helm-project-smart-do-search-in-dir my-org-directory))
    )
  )

(defun my-org/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer (spacemacs/defer)
    :config
    ;; 这个不能如果文件不存在，没有匹配项，能新建一个。
    (setq my-org-directory "~/.txnix/.org_notes/notes/")
    (comma-def
      "F" 'my/helm-locate-org-directories
      )
    ))

(defun my-org/post-init-deft ()
  (use-package deft
    :config
    ;; (comma-def "c" #'dingyi/capture-to-deft-today-file)

    (setq deft-directory my-org-notes-directory)
    ;; deft-open-file会触发自动保存。
    ;; deft-fine-file 调用deft-open-file
    ;; 所以用 ,f打开的文件会自动保存。
    ;; (comma-def "f" #'deft-find-file)
    ;; (comma-def "f" #'my/deft-find-file)

    ))

(defun my-org/post-init-helm-org-rifle ()
  (spacemacs|use-package-add-hook helm-org-rifle
    :post-config
    ;; (comma-def "r" 'helm-org-rifle-org-directory)
    ;; (comma-def "r" (lambda ()
    ;;                  (let ((org-directory "~/.txnix/.org_notes/notes/"))
    ;;                    (helm-org-rifle-org-directory))))
    ;; 太慢了。还是要用数据库技术。
    ;; (comma-def "r" (lambda ()
    ;;                  (interactive)
    ;;                  (helm-org-rifle-directories '("~/.txnix/.org_notes/notes/") t)
    ;;                  ))
    ))

;; (defun my-org/init-org-ql ()
;;   (use-package org-ql
;;     ))

;;; packages.el ends here

;;; org-insert-heading
;; (evil-org-org-insert-heading-respect-content-below)
;;(defun my-org-search-or-insert-heading ()
;;  (interactive)
;;  ;; 如果已经有了匹配的heading，就跳转
;;  (counsel-org-goto)
;;  ;; 如果没有匹配的，就在buffer最后新建一个
;;
;;  )
;;(general-def org-mode-map "<C-return>" 'my-org-search-or-insert-heading)
;;(general-def
;;  :states '(normal insert)
;;  :keymaps 'override
;;  ;; "M-f" 'counsel-outline
;;  "M-f" 'spacemacs/helm-jump-in-buffer
;;  )
;;
