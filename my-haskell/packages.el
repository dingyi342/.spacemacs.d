;;; packages.el --- my-haskell layer packages file for Spacemacs.
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
;; added to `my-haskell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-haskell/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-haskell/pre-init-PACKAGE' and/or
;;   `my-haskell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-haskell-packages
  '(
    (ob-haskell :location local)
    )
)

(defun my-haskell/init-ob-haskell ()
  "https://github.com/mikesperber/org-mode/blob/master/lisp/ob-haskell.el
org8.3,现在是9了需要修改"
  (use-package ob-haskell
    )
  )

;;; packages.el ends here
