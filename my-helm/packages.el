;;; packages.el --- my-helm layer packages file for Spacemacs.
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

;;; Code:

(defconst my-helm-packages
  '(
    helm
    helm-spacemacs-help
    ;; helm-posframe
    helm-bookmark
    helm-file-preview
    )
  )

;;; post helm

(defun my-helm/post-init-helm ()
  (use-package helm
    :config
    ;; (setq helm-display-buffer-height 10)
    ;; (helm-autoresize-mode -1)
    (helm-autoresize-mode 1)
    (setq helm-autoresize-max-height 28
          helm-autoresize-min-height 28
          )
    ;; (setq completing-read-function 'ivy-completing-read)
    ;; 使得只在又当前窗口范围内弹出buffer.
    ;; (setq helm-display-function #'helm-default-display-buffer)

    ;; (setq
    ;;  helm-etags-fuzzy-match t
    ;;  helm-use-fuzzy 'always
    ;;  helm-fuzzy-sort-fn t
    ;;  helm-ag-fuzzy-match t
    ;;  helm-fuzzy-match-fn t
    ;;  helm-fuzzy-search-fn t
    ;;  helm-M-x-fuzzy-match t
    ;;  helm-mode-fuzzy-match t
    ;;  helm-ff-fuzzy-matching t
    ;;  helm-locate-fuzzy-match t
    ;;  helm-apropos-fuzzy-match t
    ;;  helm-locate-fuzzy-sort-fn t
    ;;  helm-swoop-use-fuzzy-match t
    ;;  helm-lisp-fuzzy-completion t
    ;;  helm-projectile-fuzzy-match t
    ;;  helm-locate-library-fuzzy-match t
    ;;  helm-fuzzy-matching-highlight-fn t
    ;;  helm-completion-in-region-fuzzy-match t
    ;;  helm-buffers-fuzzy-matching t
    ;;  helm-completion-in-region-fuzzy-match t
    ;;  )

    ;; ;; 没什么用
    ;; (setq helm-mode-fuzzy-match t)
    ;; (setq helm-completion-in-region-fuzzy-match t)

    ;; 默认是100, 50可以提升速度。
    ;; (setq helm-candidate-number-limit 50)

    (spacemacs|define-transient-state dingyi-helm-navigation
      :title "Helm Transient State"
      :doc "
 [_j_/_k_]  next/prev candidate  [_v_]^^     persistent action     [_e_]^^    edit occurrences
 [_h_/_l_]  prev/next source     [_a_.._g_]  action 1..5         [_t_/_T_]  toggle visible/all mark
 [_q_]^^    quit                 [_1_]^^     action selection pg"
      :foreign-keys run
      :on-enter (spacemacs//helm-navigation-ts-on-enter)
      :on-exit  (spacemacs//helm-navigation-ts-on-exit)
      :bindings
      ("a" spacemacs/helm-action-1 :exit t)
      ("s" spacemacs/helm-action-2 :exit t) ("d" spacemacs/helm-action-3 :exit t) ("f" spacemacs/helm-action-4 :exit t) ("g" spacemacs/helm-action-5 :exit t) ("6" spacemacs/helm-action-6 :exit t)
      ("7" spacemacs/helm-action-7 :exit t)
      ("8" spacemacs/helm-action-8 :exit t)
      ("9" spacemacs/helm-action-9 :exit t)
      ("0" spacemacs/helm-action-10 :exit t)
      ("<tab>" helm-select-action :exit t)
      ("TAB" helm-select-action :exit t)
      ("<RET>" helm-maybe-exit-minibuffer :exit t)
      ;; ("?" nil :doc (spacemacs//helm-navigation-ts-full-doc))
      ("1" spacemacs/helm-transient-state-select-action)
      ("e" spacemacs/helm-ts-edit)
      ("5" helm-beginning-of-buffer)
      ("G" helm-end-of-buffer)
      ("h" helm-previous-source)
      ("j" helm-next-line)
      ("k" helm-previous-line)
      ("l" helm-next-source)
      ("q" nil :exit t)
      ("M-SPC" nil :exit t)
      ("t" helm-toggle-visible-mark)
      ("T" helm-toggle-all-marks)
      ("v" helm-execute-persistent-action))
    (define-key helm-map (kbd "<C-i>")
      'spacemacs/dingyi-helm-navigation-transient-state/body)
    ;; (define-key helm-map (kbd "C-z")
    ;;   'spacemacs/dingyi-helm-navigation-transient-state/body)

    (general-def "M-x" 'helm-M-x)
    (general-def "C-s" 'helm-swoop)
    )
  )

;;; post helm-spacemacs-help
(defun my-helm/post-init-helm-spacemacs-help ()
  (use-package helm-spacemacs-help
    :config
    ;; 覆盖，把package.el调到第一位。好像没什么用。
    (advice-add 'helm-spacemacs-help//layer-source :override 'helm-spacemacs-help//dingyi-layer-source)

    ;;; SPC h l 默认打开package.el
    (defun helm-spacemacs-help//dingyi-layer-source ()
      "Construct the helm source for the layer section."
      `((name . "Layers")
        (candidates . ,(sort (configuration-layer/get-layers-list) 'string<))
        (candidate-number-limit)
        (keymap . ,helm-spacemacs-help--layer-map)
        (action . (
                   ("Open packages.el"
                    . helm-spacemacs-help//layer-action-open-packages)
                   ("Open funcs.el"
                    . helm-spacemacs-help//layer-action-open-funcs)
                   ("Open config.el"
                    . helm-spacemacs-help//layer-action-open-config)
                   ("Open README.org"
                    . helm-spacemacs-help//layer-action-open-readme)
                   ("Open layers.el"
                    . helm-spacemacs-help//layer-action-open-layers)
                   ("Install Layer"
                    . helm-spacemacs-help//layer-action-install-layer)
                   ("Open README.org (for editing)"
                    . helm-spacemacs-help//layer-action-open-readme-edit)))))

    ))

(defun my-helm/init-helm-posframe ()
  (use-package helm-posframe
    :config
    (helm-posframe-enable)
    (setq helm-posframe-parameters
          '((left-fringe . 300)
            (right-fringe . 900)))
    (setq helm-display-function #'helm-default-display-buffer)
    )
  )

(defun my-helm/post-init-helm-bookmark ()
  (use-package helm-bookmark
    :config
    (general-def :keymaps 'override
      "M-1" 'helm-bookmarks)
    (comma-def "b" 'helm-bookmarks)
    (semicolon-def "f" 'helm-bookmarks)
    )
  )

(defun my-helm/init-helm-file-preview ()
  (use-package helm-file-preview
    :config
    (helm-file-preview-mode t)
    (setq helm-file-preview-preview-only t
          helm-file-preview-only-when-line-numbers t)
    )
  )
;;; packages.el ends here
