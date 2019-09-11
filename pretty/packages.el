;;; packages.el --- pretty layer packages file for Spacemacs.
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
;; added to `pretty-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `pretty/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `pretty/pre-init-PACKAGE' and/or
;;   `pretty/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst pretty-packages
  '(
    pretty-mode
    pretty-hydra
    prettify-greek
    pretty-symbols
    pretty-sha-path
    latex-pretty-symbols
    ipretty
    ))

(defun pretty/init-pretty-mode ()
  "https://github.com/pretty-mode/pretty-mode"
  (use-package pretty-mode
    :init
    ;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)
    :config
    (global-pretty-mode t)
    )
  )

;;; packages.el ends here
