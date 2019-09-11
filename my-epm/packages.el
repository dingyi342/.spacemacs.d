;;; packages.el --- my-epm layer packages file for Spacemacs.
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

(defconst my-epm-packages
  '(
    paradox
    ;; epm
    )
)

(defun my-epm/post-init-paradox ()
  (use-package paradox
    :config
    (setq paradox-github-token "d2377a3af441fa21ccd835375f054a39c9c017b4")
    )
  )

(defun my-epm/init-epm ()
  "command line wrapper for package.el, 其实没多大用，玩一玩"
  (use-package epm
    :config
    )
  )

;;; packages.el ends here
