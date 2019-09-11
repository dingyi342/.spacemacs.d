;;; packages.el --- my-deft layer packages file for Spacemacs.
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
;; added to `my-deft-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-deft/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-deft/pre-init-PACKAGE' and/or
;;   `my-deft/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-deft-packages
  '(
    deft
    )
)

(defun my-deft/init-deft ()
  (use-package deft
    :defer (spacemacs/defer)
    :init
    (progn
      (setq deft-extensions '("org" "md" "txt")
            deft-directory my-org-directory
            deft-text-mode 'org-mode
            ;; deft-recursive t
            deft-use-filename-as-title t
            deft-use-filter-string-for-filename t)
      (spacemacs/set-leader-keys "aod" 'spacemacs/deft)
      (comma-def
        "F" 'deft-find-file
        "N" 'deft-new-file)

      (defun spacemacs/deft ()
        "Helper to call deft and then fix things so that it is nice and works"
        (interactive)
        (deft)
        ;; Hungry delete wrecks deft's DEL override
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1))
        ;; When opening it you always want to filter right away
        (evil-insert-state nil)))
    :config (spacemacs/set-leader-keys-for-major-mode 'deft-mode
              "c" 'deft-filter-clear
              "d" 'deft-delete-file
              "i" 'deft-toggle-incremental-search
              "n" 'deft-new-file
              "N" 'deft-new-file-named
              "q" 'quit-window
              "o" 'deft-open-file-other-window
              "r" 'deft-rename-file)))

;;; packages.el ends here
