;;; packages.el --- firefox layer packages file for Spacemacs.
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

(defconst my-firefox-packages
  '(
    ;; firefox-controller
    ;; helm-firefox
    ;; exwm-firefox-core
    ;; exwm-firefox-evil
    counsel-ffdata                      ;访问firefox书签和历史,非常快.
    )
)

;;;; 用emacs控制 firefox
;; 现在不管用了啊.
(defun my-firefox/init-firefox-controller ()
  (use-package firefox-controller
    :config

    ))

(defun my-firefox/init-exwm-firefox-core ()
  (use-package exwm-firefox-core))

(defun my-firefox/init-exwm-firefox-evil ()
  (use-package exwm-firefox-evil
    :init
    ;; Auto enable exwm-firefox-evil-mode on all firefox buffers
    (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
    ))
;;;; counsel-ffdata 访问firefox书签和历史
(defun my-firefox/init-counsel-ffdata ()
  (use-package counsel-ffdata
    ;; 用 yay -Qe sqlite都查不到sqlite,这不怪emacs应该是系统的问题.
    ;; :ensure-system-package sqlite
    :config
    (spc-def "af" 'counsel-ffdata-firefox-bookmarks)
    (spc-def "ah" 'counsel-ffdata-firefox-history)
    )
  )

;;; packages.el ends here
