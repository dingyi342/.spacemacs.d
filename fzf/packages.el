;;; packages.el --- fzf layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `fzf-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `fzf/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `fzf/pre-init-PACKAGE' and/or
;;   `fzf/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst fzf-packages
  '(
    fzf
    ;; helm-fuzzy-find
    (helm-fzf :location (recipe :fetcher github
                                :repo "ibmandura/helm-fzf"))
    ;; counsel-fzf
    )
)

(defun fzf/init-fzf ()
  "M-x fzf"
  (use-package fzf)
  )

(defun fzf/init-helm-fzf ()
  "M-x helm-fzf"
  (use-package helm-fzf)
  )

;; (defun fzf/init-counsel-fzf)



;; (use-package helm
;;   :init
;;   (setq helm-semantic-fuzzy-match t)
;;   (setq helm-recentf-fuzzy-match t)
;;   (setq helm-locate-fuzzy-match t)
;;   (setq  helm-M-x-fuzzy-match t)
;;   (setq helm-imenu-fuzzy-match t)
;;   (setq helm-buffers-fuzzy-matching t)
;;   (setq helm-mode-fuzzy-match t)
;;   (setq helm-completion-in-region-fuzzy-match t)
;;   )
;;; packages.el ends here
