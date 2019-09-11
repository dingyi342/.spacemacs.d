;;; packages.el --- my-git layer packages file for Spacemacs.
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
;; magit-diff-unstaged

;;; Code:

(defconst my-git-packages
  '(
    magit-todos
    ;;magit-org-todos
    )
)

(defun my-git/init-magit-todos ()
  (use-package magit-todos
    :config
    (magit-todos-mode)
    )
  )

(defun my-git/init-magit-org-todos ()
  (use-package magit-org-todos
    :config
    )
  )

;;; packages.el ends here
