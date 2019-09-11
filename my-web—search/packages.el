;;; packages.el --- my-web—search layer packages file for Spacemacs.
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

(defconst my-web—search-packages
  '(
    helm-youtube
    )
)

(defun my-web-search/init-helm-youtube ()
  (use-package helm-youtube
    :config
    (require 'helm-youtube)
    (setq helm-youtube-key "AIzaSyAxz8FcYkfQuM8yyQkPyYh7lyZlcWO8_Ew")
    )
  )

;;; packages.el ends here
