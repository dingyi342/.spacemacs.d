;;; packages.el --- my-shell layer packages file for Spacemacs.
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
;; 基本上用不到  vterm,因为我系统的终端按win一下就召唤出来了，很方便。
;; 这个优势就是可以和emacs更好的合作啊。
;; 而且这个实现方式也很值得学习
;;; Code:

(defconst my-shell-packages
  '(
    vterm
    ;; (vterm :location (recipe :fetcher github
                             ;; :repo "jixiuf/emacs-libvterm"))
    (vterm-toggle :location (recipe :fetcher github
                                    :repo "jixiuf/vterm-toggle"))
    )
)

(defun my-shell/init-vterm ()
  (use-package vterm
    ;; :ensure-system-package (libvterm)
    :config
    (general-def
      :keymaps 'vterm-mode-map
      :states 'insert

      )

    ))

(defun my-shell/init-vterm-toggle ()
  (use-package vterm-toggle
    :config
    ;; vterm-toggle
    ;; vterm-toggle-cd
    ;; vterm-toggle-forward
    ;; vterm-toggle-backward
    (general-def
      :states 'normal
      "<f12>" 'vterm-toggle
      "<f2>" 'vterm-toggle
      "M-v" 'vterm-toggle-cd
      )
    )
  )
;;; packages.el ends here
