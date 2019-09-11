;;; packages.el --- academic layer packages file for Spacemacs.
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

(defconst academic-packages
  '(
    (helm-bibtex :require helm)
    (ivy-bibtex :require)
    ivbibtex-utils
    company-bibtex
    gscholar-bibtex
    helm-bibtexkey
    ox-bibtex-chinesey
    org-noter
    )
)

;;; org-noter
;; https://github.com/weirdNox/org-noter
(defun academic/init-org-noter ()
  (use-package org-noter
    ))
;;; packages.el ends here
