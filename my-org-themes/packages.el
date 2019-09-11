;;; packages.el --- my-org-themes layer packages file for Spacemacs.
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

(defconst my-org-themes-packages
  '(
    (my-org-prettify-blocks :location local)
    )
)

(defun my-org-themes/init-my-org-prettify-blocks ()
  (use-package my-org-prettify-blocks
    ;; :defer (spacemacs/defer)
    :config
    )
  )

;;; packages.el ends here
