;;; packages.el --- input-method layer packages file for Spacemacs.
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
;; added to `input-method-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `input-method/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `input-method/pre-init-PACKAGE' and/or
;;   `input-method/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst input-method-packages
  '(
   ;; pyim
   ;; flypy
   (exim :location (recipe :fetcher github
                           :repo "ch11ng/exim"))
   )
  )

(defun input-method/init-exim ()
  (require 'exim)
  (add-hook 'exwm-init-hook 'exim-start)
  (push ?\C-\\ exwm-input-prefix-keys)
  )


;;; packages.el ends here
