;;; packages.el --- my-ivy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <my@my>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(setq my-ivy-packages
      '(
        auto-highlight-symbol
        bookmark
        counsel
        counsel-projectile
        evil
        flx
        ;; helm-make
        imenu
        ivy
        ivy-hydra
        (ivy-rich :toggle ivy-enable-advanced-buffer-information)
        (ivy-spacemacs-help :location local)
        ivy-xref
        org
        persp-mode
        projectile
        recentf
        smex
        swiper
        wgrep
        ivy-posframe
        ))

(defun my-ivy/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swiper  [_b_] buffers  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" spacemacs/swiper-region-or-symbol :exit t)
        ("b" spacemacs/swiper-all-region-or-symbol :exit t)
        ("f" spacemacs/search-auto-region-or-symbol :exit t)
        ("/" spacemacs/search-project-auto-region-or-symbol :exit t)))))

(defun my-ivy/post-init-bookmark ()
  (spacemacs/set-leader-keys "fb" 'counsel-bookmark))

(defun my-ivy/init-counsel ()
  (use-package counsel
    :init
    (progn
      (sspc-def
        dotspacemacs-emacs-command-key 'counsel-M-x
        ;; files
        "ff"  'counsel-find-file
        "fel" 'counsel-find-library
        "fL"  'counsel-locate
        ;; help
        "?"   'counsel-descbinds
        "hda" 'counsel-apropos
        "hdf" 'counsel-describe-function
        "hdF" 'counsel-describe-face
        "hdm" 'spacemacs/describe-mode
        "hdv" 'counsel-describe-variable
        "hi"  'counsel-info-lookup-symbol
        "hR"  'spacemacs/counsel-search-docs
        ;; insert
        "iu"  'counsel-unicode-char
        ;; jump
        ;; register/ring
        "ry"  'counsel-yank-pop
        "rm"  'counsel-mark-ring
        ;; jumping
        "sj"  'spacemacs/counsel-jump-in-buffer
        ;; themes
        "Ts"  'counsel-load-theme
        ;; search
        "/"   'spacemacs/search-project-auto
        "*"   'spacemacs/search-project-auto-region-or-symbol
        "sd"  'spacemacs/search-dir-auto
        "sD"  'spacemacs/search-dir-auto-region-or-symbol
        "sf"  'spacemacs/search-auto
        "sF"  'spacemacs/search-auto-region-or-symbol
        "sp"  'spacemacs/search-project-auto
        "sP"  'spacemacs/search-project-auto-region-or-symbol
        "sad" 'spacemacs/search-dir-ag
        "saD" 'spacemacs/search-dir-ag-region-or-symbol
        "saf" 'spacemacs/search-ag
        "saF" 'spacemacs/search-ag-region-or-symbol
        "sap" 'spacemacs/search-project-ag
        "saP" 'spacemacs/search-project-ag-region-or-symbol
        "sgd" 'spacemacs/search-dir-grep
        "sgD" 'spacemacs/search-dir-grep-region-or-symbol
        "sgf" 'spacemacs/search-grep
        "sgF" 'spacemacs/search-grep-region-or-symbol
        "sgp" 'counsel-git-grep
        "sgP" 'spacemacs/counsel-git-grep-region-or-symbol
        "skd" 'spacemacs/search-dir-ack
        "skD" 'spacemacs/search-dir-ack-region-or-symbol
        "skf" 'spacemacs/search-ack
        "skF" 'spacemacs/search-ack-region-or-symbol
        "skp" 'spacemacs/search-project-ack
        "skP" 'spacemacs/search-project-ack-region-or-symbol
        "srd" 'spacemacs/search-dir-rg
        "srD" 'spacemacs/search-dir-rg-region-or-symbol
        "srf" 'spacemacs/search-rg
        "srF" 'spacemacs/search-rg-region-or-symbol
        "srp" 'spacemacs/search-project-rg
        "srP" 'spacemacs/search-project-rg-region-or-symbol
        "std" 'spacemacs/search-dir-pt
        "stD" 'spacemacs/search-dir-pt-region-or-symbol
        "stf" 'spacemacs/search-pt
        "stF" 'spacemacs/search-pt-region-or-symbol
        "stp" 'spacemacs/search-project-pt
        "stP" 'spacemacs/search-project-pt-region-or-symbol))
    :config
    (progn
      ;; Temporarily handle older versions of ivy
      ;; https://github.com/abo-abo/swiper/pull/1863/files
      (unless (fboundp 'counsel--elisp-to-pcre)
        (defalias 'counsel--elisp-to-pcre 'counsel-unquote-regex-parens))

      ;; set additional ivy actions
      (ivy-set-actions
       'counsel-find-file
       spacemacs--ivy-file-actions)

      (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
      (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
      ;; remaps built-in commands that have a counsel replacement
      (counsel-mode 1)
      (spacemacs|hide-lighter counsel-mode)
      ;; TODO Commands to port
      (spacemacs//ivy-command-not-implemented-yet "jI")
      ;; Set syntax highlighting for counsel search results
      (ivy-set-display-transformer 'spacemacs/counsel-search 'counsel-git-grep-transformer))))

(defun my-ivy/pre-init-counsel-projectile ()
  ;; overwrite projectile settings
  (spacemacs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)

      (ivy-set-actions
       'counsel-projectile-find-file
       (append spacemacs--ivy-file-actions
               '(("R" (lambda (arg)
                        (interactive)
                        (call-interactively
                         #'projectile-invalidate-cache)
                        (ivy-resume)) "refresh list")
                 )))

      (sspc-def
        "p SPC" 'counsel-projectile
        "pb"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file))))

(defun my-ivy/post-init-evil ()
  (sspc-def
    "re" 'spacemacs/ivy-evil-registers))

(defun my-ivy/init-flx ()
  (use-package flx))

;; (defun my-ivy/init-helm-make ()
;;   (use-package helm-make
;;     :init
;;     (progn
;;       (setq helm-make-completion-method 'ivy)
;;       (sspc-def
;;         "cc" 'helm-make-projectile
;;         "cm" 'helm-make))))

(defun my-ivy/post-init-imenu ()
  (sspc-def "ji" 'spacemacs/counsel-jump-in-buffer))

(defun my-ivy/init-ivy ()
  (use-package ivy
    :init
    (progn
      ;; 使用ivy来不全 completing-read，比如deft-find-file,
      ;; (setq completing-read-function 'ivy-completing-read)
      ;; Key bindings
      (sspc-def
        "a'" 'spacemacs/ivy-available-repls
        "fr" 'counsel-recentf
        "rl" 'ivy-resume
        "bb" 'ivy-switch-buffer))

    :config
    (progn
      ;; custom actions for recentf
      (ivy-set-actions
       'counsel-recentf
       spacemacs--ivy-file-actions)

      ;; add spacemacs/counsel-search command to ivy-highlight-grep-commands
      (add-to-list 'ivy-highlight-grep-commands 'spacemacs/counsel-search)

      ;; mappings to quit minibuffer or enter transient state
      (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
      (define-key ivy-minibuffer-map (kbd "M-SPC") 'hydra-ivy/body)

      (when ivy-ret-visits-directory
        (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
        (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-done))

      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "<f6>") 'ivy-resume)
      ;; Occur
      (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      (ivy-set-occur 'spacemacs/counsel-search
                     'spacemacs//counsel-occur)
      (sspc-def-for-major-mode 'ivy-occur-grep-mode
        "w" 'spacemacs/ivy-wgrep-change-to-wgrep-mode
        "s" 'wgrep-save-all-buffers)
      ;; Why do we do this ?
      (ido-mode -1)

      ;; allow to select prompt in some ivy functions
      (setq ivy-use-selectable-prompt t))))

(defun my-ivy/init-ivy-hydra ()
  (use-package ivy-hydra)
  (define-key hydra-ivy/keymap [escape] 'hydra-ivy/keyboard-escape-quit-and-exit))

(defun my-ivy/init-ivy-rich ()
  (use-package ivy-rich
    ;; if `counsel' loads after `ivy-rich', it overrides some of `ivy-rich''s
    ;; transformers
    :after counsel
    :init
    (progn
      (setq ivy-rich-path-style 'abbrev
            ivy-virtual-abbreviate 'full))
    :config
    (progn
      (ivy-rich-mode))))

(defun my-ivy/init-ivy-spacemacs-help ()
  (use-package ivy-spacemacs-help
    :commands (ivy-spacemacs-help-dotspacemacs
               ivy-spacemacs-help
               ivy-spacemacs-help-faq
               ivy-spacemacs-help-layers
               ivy-spacemacs-help-packages
               ivy-spacemacs-help-docs
               ivy-spacemacs-help-toggles)
    :init (sspc-def
            "h ."   'ivy-spacemacs-help-dotspacemacs
            "h SPC" 'ivy-spacemacs-help
            "h f"   'ivy-spacemacs-help-faq
            "h l"   'ivy-spacemacs-help-layers
            "h p"   'ivy-spacemacs-help-packages
            "h r"   'ivy-spacemacs-help-docs
            "h t"   'ivy-spacemacs-help-toggles)))

(defun my-ivy/init-ivy-xref ()
  (use-package ivy-xref
    :defer (spacemacs/defer)
    :init
    (progn
      (setq xref-prompt-for-identifier '(not xref-find-definitions
                                             xref-find-definitions-other-window
                                             xref-find-definitions-other-frame
                                             xref-find-references
                                             spacemacs/jump-to-definition))

      ;; Use ivy-xref to display `xref.el' results.
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))))

(defun my-ivy/post-init-org ()
  (add-hook 'org-ctrl-c-ctrl-c-hook 'spacemacs//counsel-org-ctrl-c-ctrl-c-org-tag))

(defun my-ivy/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (setq
     spacemacs--persp-display-buffers-func 'spacemacs/ivy-spacemacs-layout-buffer
     spacemacs--persp-display-perspectives-func 'spacemacs/ivy-spacemacs-layouts)))

(defun my-ivy/post-init-persp-mode ()
  ;; based on https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (add-hook 'ivy-ignore-buffers #'spacemacs//layout-not-contains-buffer-p)
  (setq ivy-sort-functions-alist
        (append ivy-sort-functions-alist
                '((persp-kill-buffer . nil)
                  (persp-remove-buffer . nil)
                  (persp-add-buffer . nil)
                  (persp-switch . nil)
                  (persp-window-switch . nil)
                  (persp-frame-switch . nil))))

  (ivy-set-actions
   'spacemacs/ivy-spacemacs-layouts
   '(("c" persp-kill-without-buffers "Close layout(s)")
     ("k" persp-kill  "Kill layout(s)")
     ("n" persp-copy "Copy Current Layout")
     ("p" spacemacs//create-persp-with-current-project-buffers
      "Create Project Layout")))
  ;; TODO: better handling of C and X bindings for ivy
  ;;       check ivy/pre-init-persp-mode
  (spacemacs/transient-state-register-remove-bindings 'layouts
    '("C" "X"))
  (spacemacs/transient-state-register-add-bindings 'layouts
    '(("C" spacemacs/ivy-spacemacs-layout-close-other :exit t)
      ("X" spacemacs/ivy-spacemacs-layout-kill-other :exit t))))

(defun my-ivy/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (sspc-def
    "pv"  'projectile-vc))

(defun my-ivy/post-init-recentf ()
  ;; custom actions for recentf
  (ivy-set-actions
   'counsel-recentf
   (append spacemacs--ivy-file-actions
           '(("R" (lambda (arg)
                    (interactive)
                    (recentf-cleanup)
                    (ivy-recentf)) "refresh list")
             ("D" (lambda (arg)
                    (interactive)
                    (setq recentf-list (delete arg recentf-list))
                    (ivy-recentf)) "delete from list"))))
  ;; merge recentf and bookmarks into buffer switching. If we set this
  (setq ivy-use-virtual-buffers t))

(defun my-ivy/init-smex ()
  (use-package smex
    :defer (spacemacs/defer)
    :init (setq-default smex-history-length 32
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items"))))

(defun my-ivy/init-swiper ()
  (use-package swiper
    :config
    (progn
      (sspc-def
        "ss" 'swiper
        "sS" 'spacemacs/swiper-region-or-symbol
        "sb" 'swiper-all
        "sB" 'spacemacs/swiper-all-region-or-symbol)
      (global-set-key "\C-s" 'swiper))))

(defun my-ivy/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))

(defun my-ivy/init-ivy ()
  (use-package ivy
    :config
    ;; (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          )
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "C-c b") 'ivy-switch-buffer)
        ))

(defun my-ivy/init-counsel ()
  (use-package counsel
    :config
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ;; (global-set-key (kbd "M-x") 'counsel-M-x)

    ))

(defun my-ivy/init-ivy-posframe ()
  "在exwm的buffer上不能显示。"
  (require 'ivy-posframe)
  ;; display at `ivy-posframe-style'
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  (ivy-posframe-mode -1)
  )
