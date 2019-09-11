;;; packages.el --- my-snippet layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: dingyi <dingyi@dingyi>
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
;; added to `my-snippet-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-snippet/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-snippet/pre-init-PACKAGE' and/or
;;   `my-snippet/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-snippet-packages
  '(
    yasnippet
    ;; yasnippet-snippets
    ;; yasnippet-classic-snippets
    )
)

(defun my-snippet/post-init-yasnippet ()
  (use-package yasnippet
    :config
    (yas-global-mode 1)
    ;; (setq my-spacemacs-directory "~/.spacemacs.d/")
    ;; (setq my-snippets-dir (concat my-spacemacs-directory "snippets/"))
    (setq yas-snippet-dirs nil)
    (setq my-yasnippet-snippets "~/.spacemacs.d/snippets/")
    (add-to-list 'yas-snippet-dirs 'my-yasnippet-snippets t)
    (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
    (yas-load-directory my-yasnippet-snippets t)
    )
  )

(defun my-snippet/post-init-yasnippet-snippets ()
  (use-package yas-snippet
    :after yasnippet
    :config
    (unless (memq 'yasnippet-snippets-dir yas-snippet-dirs)
      ;; Prepare for future snippet reloads.
      (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
      ;; And get our snippets ready now.
      (yas-load-directory yasnippet-snippets-dir t))
    )
  )

(defun my-snippet/init-yasnippet-classic-snippets ()
  (use-package yasnippet-classic-snippets
    :after yasnippet
    :config
    (progn
      (when (stringp yas-snippet-dirs)
        ;; In case the user set the old format.
        (setq yas-snippet-dirs (list yas-snippet-dirs)))
      (unless (memq 'yasnippet-classic-snippets-dir yas-snippet-dirs)
        ;; Prepare for future snippet reloads.
        (add-to-list 'yas-snippet-dirs 'yasnippet-classic-snippets-dir t)
        ;; And get our snippets ready now.
        (yas-load-directory yasnippet-classic-snippets-dir t)))
    )
  )
;;; packages.el ends here
