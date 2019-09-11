;;; packages.el --- outshine layer packages file for Spacemacs.
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
;; added to `outshine-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `outshine/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `outshine/pre-init-PACKAGE' and/or
;;   `outshine/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst outshine-packages
  '(
    (outline :location built-in)
    outshine
    outorg
    navi-mode
    ;; (helm-navi :require helm)  ;有问题不合并pr.
    (helm-navi :location local)
    ;; (helm-navi :location (recipe :fetcher github
                               ;; :repo "dingyi342/helm-navi"))
    (outline-ivy :location local)
    )
)

;;; outline
  (defun outshine/init-outline ()
    (use-package outline
      :defer (spacemacs/defer)
      :init
      (defvar outline-minor-mode-prefix "\M-#") ;must be set before outline is loaded
      )
    )

;;; outshine
  (defun outshine/init-outshine ()
    (use-package outshine
      :defer (spacemacs/defer)
      :init
      (add-hook 'outline-minor-mode-hook #'outshine-mode)
      (add-hook 'prog-mode-hook #'outline-minor-mode)
      (add-hook 'prog-mode-hook #'outshine-mode)
      :config
      (setq outshine-use-speed-commands t)

      ;; Narrowing now works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  (lambda (&rest args) (unless (outline-on-heading-p t)
                                         (outline-previous-visible-heading 1))))
      (spacemacs/set-leader-keys
        ;; Narrowing
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen

        "nj" 'outline-move-subtree-down
        "nk" 'outline-move-subtree-up
        "nj" 'outline-promote
        "nl" 'outline-demote
        )
;;;; keybindings
      (general-def
        :keympas 'outline-minor-mode-map
        "M-RET" 'outshine-insert-heading
        "<backtab>" 'outshine-cycle-buffer)

      (general-def
        :keymaps 'outline-minor-mode-map
        :states '(normal)
        "M-RET" (lambda ()
                  (interactive)
                  (outshine-insert-heading)
                  (evil-insert-state)
                  ))

      (general-def
        :states '(normal visual motion)
        :keymaps 'outline-minor-mode-map
        :prefix "g"
        "h" 'outline-up-heading
        "j" 'outline-forward-same-level
        "k" 'outline-backward-same-level
        "l" 'outline-next-visible-heading
        "u" 'outline-previous-visible-heading)
      ;; (comma-def outshine-mode-map "j" 'outshine-imenu)
      ;; (comma-def outline-minor-mode-map "j" 'outshine-imenu)

;;;; 设置headline trail
      (defvar outline-display-table (make-display-table))
      (set-display-table-slot outline-display-table 'selective-display
                              (vector (make-glyph-code ?▼ 'escape-glyph)))
      (defun set-outline-display-table ()
        (setf buffer-display-table outline-display-table))

      (add-hook 'outline-mode-hook 'set-outline-display-table)
      (add-hook 'outline-minor-mode-hook 'set-outline-display-table)
      )
;;;; end
    )

;;; outorg
  (defun outshine/init-outorg ()
    " M-# # outorg-edit-as-org 打开 outorg-edit-buffer,激活 outorg-edit-minor-mode
https://github.com/alphapapa/outorg
"
    (use-package outorg
      :defer (spacemacs/defer)
      )
    )

;;; navi
; outshine-navi
  (defun outshine/init-navi-mode ()
    "https://github.com/alphapapa/navi"
    (use-package navi-mode
      ;outshine-navi 会新开一个buffer显示outshine的headline.
      :defer (spacemacs/defer)
      )
    )

;;; helm-navi
;; outline-promotion已经被新版的outshine用outshine-promotion替代了。直接替换就好了。
;; helm-navi就没outshine-ivy那样的注释问题。
;; 老是不合并pr，用本地版本。
;; https://github.com/emacs-helm/helm-navi
(defun outshine/init-helm-navi ()
  (use-package navi-mode
    :defer (spacemacs/defer)
    :config
    ;; Add "use-package" lines to `navi-keywords'.
    (setf (cdr (assoc :ALL (cdr (assoc "emacs-lisp" navi-keywords))))
          "^[[:space:]]*(\\(use-package\\|\\(cl-\\)\\{0,1\\}def[a-z]+\\)\\*? ")
    ;,j"n" 'helm-navi-headings
    ;,j"N" 'helm-navi
    )
  )
;;; outline-ivy
; https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/personal/packages.el
; http://www.modernemacs.com/post/outline-ivy/
;; 如果注释不是开头就是分号，但是有3个及以上的分好，就不会被识别为headline，但是outline-ivy会识别，但是又不认为是合法的输入，就会报错，错误的类型。可以设置必须从头开始的才合法。
;;   ;; ;; * xxx 这样也是不合法的，会有报错信息，看定位到哪里就好了。
(defun outshine/init-outline-ivy ()
  (use-package outline-ivy
    :defer (spacemacs/defer)
    :commands oi-jump
    ;; :init
    ;; ("C-j" . oi-jump)
    ;keybindings
    ;,jj 'oi-jump
  )
  )

;;; packages.el ends here
