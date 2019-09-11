;;; packages.el --- my-modeline layer packages file for Spacemacs.
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
;; spacemacs/toggle-mode-line-off
;; spacemacs/toggle-mode-line-on
;; (hidden-mode-line-mode -1)


;;; Code:

(defconst my-modeline-packages
  '(
    ;; ert-modeline
    ;; sml-modeline
    ;; ocodo-svg-modelines
    ;; mode-line-bell
    ;; mode-line-debug
    hide-mode-line
    ;; smart-mode-line
    ;; celestial-mode-line                 ;显示月相，日出时间
    ;; emms-mode-line-cycle
    ;; svg-mode-line-themes
    ;; smart-mode-line-powerline-theme
    ;; tdd-status-mode-line
    ;; flycheck-color-mode-line
    ;; smart-mode-line-atom-one-dark-theme
    ;; doom-modeline
    ;; spaceline
    ;; diminish                            ;minor mode for hiding modeline something
    ;; feebleline
    (awesome-tray :location (recipe :fetcher github
                                    :repo "manateelazycat/awesome-tray"))
    )
)

(defun my-modeline/init-hide-mode-line ()
  (use-package hide-mode-line
    :init
    ;; (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
    ;; (add-hook 'neotree-mode-hook #'hide-mode-line-mode)
    :config
    ;; replace the mode-line in specified windows
    ;; (let ((hide-mode-line-format '("%b")))
    ;;   (hide-mode-line-mode +1))

    ;; (setq-local hide-mode-line-format '("%b"))
    ;; (hide-mode-line-mode +1)

    ;; 新建一个global minor mode
    (define-globalized-minor-mode my-global-hide-mode-line-mode hide-mode-line-mode
      (lambda ()
        (hide-mode-line-mode +1)
        ;; 隐藏modeline就可以让minibuffer跳了
        ;; (setq resize-mini-windows 'grow-only)
        ))
    (my-global-hide-mode-line-mode +1)

    (spacemacs/set-leader-keys "Tl" 'hide-mode-line-mode)
    )
  )


;;; awesome-tray
(defun my-modeline/init-awesome-tray ()
  "还蛮好看的，spacemacs可以用toggle-mode-line来隐藏，如果要显示modeline的话有点不好看，可以在awesome-tray里设置一下不要对mode-line做修改。由用户自己决定是否显示modeline.
其次,M-x 输入的字符看不见啊。"
  (use-package awesome-tray
    :config
    ;; (setq awesome-tray-active-modules '("date"))
    (dingyi/awesome-tray-enable)
    ;; (setq awesome-tray-info-padding-right 0)
    )
  )

;;; packages.el ends here
