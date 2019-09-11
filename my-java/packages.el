;;; packages.el --- my-java layer packages file for Spacemacs.
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

;;; Code:

(defconst my-java-packages
  '(
    (beautify-java :location local)
    (my-ob-java :location local) ;能指定class生成的路径
    ;; lsp-java
    )
)

(defun my-java/init-beautify-java ()
  (use-package beautify-java
    :config
    ;; add-hook 保存java-mode的文件的时候，调用 (beautify-java)
    ;; (defun my-java/return-with-format ()
    ;;   (interactive)
    ;;   (newline-and-indent)
    ;;   (save-buffer)
    ;;   (lsp-format-buffer)
    ;;   )
    ;; (general-def "RET" 'my-java/return-with-format)
    ))

(defun my-java/init-my-ob-java ()
  (use-package my-ob-java
    :config
    (advice-add 'org-babel-execute:java :override 'my-org-babel-execute:java)
    )
  )

;; (defun my-java/post-init-lsp-java ()
;;   (use-package lsp-java
;;     :config
;;     (general-def
;;       :keymaps 'java-mode-map
;;       :states '(normal insert)
;;       "M-s" (lambda ()
;;               (interactive)
;;               (lsp-format-buffer)
;;               (save-buffer)
;;               (magit-diff-unstaged)
;;               )
;;       )
;;     )
;;   )

;;; packages.el ends here

