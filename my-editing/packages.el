;;; packages.el --- my-editing layer packages file for Spacemacs.
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
;; pagngu-spacing 有性能问题,不用.,

;;; Code:

(defconst my-editing-packages
  '(
    avy
    visual-regexp-steroids
    ;; pangu-spacing
    tiny
    scratch
    )
)

(defun my-editing/post-init-avy ()
  (use-package avy
    :config
    ;; (general-nvmap :keymaps 'override "s" 'avy-goto-char-2)
    ))

(defun my-editing/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/replace
               vr/query-replace
               vr/mc-mark
               vr/isearch-backward
               vr/isearch-forward)
    ;; :config
    ))

;; (defun my-editing/init-pangu-spacing ()
;;   (use-package pangu-spacing
;;			:defer (spacemacs/defer)
;;     :init
;;     (progn
;;       (global-pangu-spacing-mode 1)
;;       (spacemacs|hide-lighter pangu-spacing-mode)
;;       ;; Always insert `real' space in org-mode.
;;       (add-hook 'org-mode-hook
;;                 '(lambda ()
;;                    (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
;;       )
;;     )
;;   )

(defun my-editing/init-tiny ()
  (use-package tiny
    :ensure t
    :config
    (tiny-setup-default)
    )
  )

(defun my-editing/init-scratch ()
  (use-package scratch
    :config
    (define-key (current-global-map) "\C-cs" #'scratch)
    )
  )

;;; packages.el ends here
