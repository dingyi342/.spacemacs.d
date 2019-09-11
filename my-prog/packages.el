;;; packages.el --- my-prog layer packages file for Spacemacs.
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

;;; Code:

(defconst my-prog-packages
  '(
    electric-operator
    electric-spacing
    electric-case
    ;; smartparens
    embrace
    evil-embrace
    evil-surround
    (my-surround :location local)
    )
)

(defun my-prog/init-electric-operator ()
  (use-package electric-operator
    :init
    (add-hook 'python-mode-hook #'electric-operator-mode)
    (add-hook 'java-mode-hook #'electric-operator-mode)
    :config
    ;; (setq
    ;; electric-operator-c-pointer-type-style
    ;; electric-operator-mode
    ;; electric-operator-enable-in-docs
    ;; electric-operator-double-space-docs
    ;; electric-operator--mode-rules-table
    ;; electric-operator-mode-hook
    ;; electric-operator-c-user-types-regex
    ;; cl-struct-electric-operator--trie-tags
    ;; electric-operator-R-named-argument-style
    ;; cl-struct-electric-operator-compiled-rule-tags
    ;; electric-operator-c-function-definition-syntax-list
    ;; electric-operator-haskell-mode-infix-binary-operators
    ;; electric-operator-haskell-mode-special-infix-binary-operators
     ;; )

    ;; rules
    (electric-operator-add-rules-for-mode 'python-mode
                                          (cons "->" " -> ")
                                          (cons "=>" " => ")
                                          )

    ;; c/java等不好区别 * 是指针类型还是乘，所以*要加个空格，才能触发
    (electric-operator-add-rules-for-mode 'c-mode
                                          (cons "*" " * "))
    (electric-operator-add-rules-for-mode 'java-mode
                                          (cons "*" " * ")
                                          (cons "/" " / ")
                                          (cons "," ", ")
                                          (cons ":" " : ")
                                          )

    ;; (apply #'electric-operator-add-rules-for-mode 'java-mode
    ;;        (electric-operator-get-rules-for-mode 'python-mode))

    ))

(defun my-prog/init-electric-spacing ()
  (use-package electric-spacing
    :config
    ))

(defun my-prog/init-electric-case ()
  "CamelCase"
  (use-package electric-case
    :init
    (add-hook 'java-mode-hook 'electric-case-java-init)
    :config
    (setq
     ;; electric-case-mode
     ;; electric-case-version

     electric-case-convert-calls t      ;头尾有数字，连字符就不转换
     electric-case-convert-beginning nil
     electric-case-convert-end nil
     electric-case-convert-nums nil

     ;; electric-case-max-iteration
     ;; electric-case-criteria
     ;; electric-case-mode-hook

     ;; electric-case--overlays
     electric-case-pending-overlay t    ;nil 'highlight

     ;; electric-case-java-primitives
     )
    ))

(defun my-prog/init-embrace ()
  (use-package embrace
    :config
    (general-def "C-," 'embrace-commander)
    (add-hook 'org-mode-hook
              (lambda ()
                (embrace-add-pair ?c "{{c1::" "}}")))
    )
  )

(defun my-prog/init-evil-embrace ()
  (use-package evil-embrace
    :init
    (add-hook 'org-mode-hook 'embrace-org-mode-hook)
    :config
    (evil-embrace-enable-evil-surround-integration)
    (setq evil-embrace-show-help-p t)
    (add-hook 'org-mode-hook
              (lambda ()
                (embrace-add-pair ?c "{{c1::" "}}")))
    )
  )

(defun my-prog/post-init-evil-surround ()
  (use-package evil-surround
    :config
    )
  )

(defun my-prog/init-my-surround ()
  (use-package my-surround
    :config

    ;; (defun tag-word-or-region (text-begin text-end)
    ;;   "Surround current word or region with given text."
    ;;   (interactive "sStart tag: \nsEnd tag: ")
    ;;   (let (pos1 pos2 bds)
    ;;     (if (and transient-mark-mode mark-active)
    ;;         (progn
    ;;           (goto-char (region-end))
    ;;           (insert text-end)
    ;;           (goto-char (region-beginning))
    ;;           (insert text-begin))
    ;;       (progn
    ;;         (setq bds (bounds-of-thing-at-point 'symbol))
    ;;         (goto-char (cdr bds))
    ;;         (insert text-end)
    ;;         (goto-char (car bds))
    ;;         (insert text-begin)))))

    )
  )
;;; packages.el ends here
