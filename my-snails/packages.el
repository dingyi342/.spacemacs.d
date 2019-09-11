;;; packages.el --- my-snails layer packages file for Spacemacs.
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

(defconst my-snails-packages
  '(
    (snails :location (recipe :fetcher github
                              :repo "manateelazycat/snails"))
    )
)

(defun my-snails/init-snails ()
  (use-package snails
    :config
    ;; M-x snails
    ;; M-x snails-search-poing
    (snails '(snails-backend-awesome-tab-group
              snails-backend-buffer
              snails-backend-rg-search
              ))

    )
  )

;;; packages.el ends here
