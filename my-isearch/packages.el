;;; packages.el --- my-isearch layer packages file for Spacemacs.
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
;; isearch,交互式搜索，不仅仅是内置的 isearch

;;; Code:

(defconst my-isearch-packages
  '(
    swiper
    (isearch :location builtin)
    helm-swoop
    symbol-overlay
    )
)

(defun my-isearch/post-init-swiper ()
  (use-package swiper
    :config

    )
  )

(defun my-isearch/post-init-iserach ()
  (use-package iserach
    :config

    (defun swiper-isearch-from-isearch ()
      "Invoke `swiper-isearch' from isearch."
      (interactive)
      (let ((query (if isearch-regexp
                       isearch-string
                     (regexp-quote isearch-string))))
        (isearch-exit)
        (swiper-isearch query)))

    ;; (define-key isearch-mode-map (kbd "<C-return>") 'swiper-isearch-isearch)
    (define-key isearch-mode-map (kbd "<C-return>") 'swiper-isearch-from-isearch)
    (general-def "C-s" 'isearch-forward)

    (defvar my-last-swiper-to-counsel-rg-search ""
      "Save current swiper input for using in `swiper-toggle-counsel-rg'.")

    (defun swiper-toggle-counsel-rg ()
      "Toggle `counsel-rg' with current swiper input."
      (interactive)
      (let ((text (replace-regexp-in-string
                   "\n" ""
                   (replace-regexp-in-string "^.*Swiper: " "" (thing-at-point 'line t)))))
        (setq my-last-swiper-to-counsel-rg-search text)
        (ivy-quit-and-run
          (counsel-rg my-last-swiper-to-counsel-rg-search default-directory))))

    (defun swiper-toggle-color-rg ()
      "Toggle `color-rg' with current swiper input."
      (interactive)
      (let ((text (replace-regexp-in-string
                   "\n" ""
                   (replace-regexp-in-string "^.*Swiper: " "" (thing-at-point 'line t)))))
        (setq my-last-swiper-to-counsel-rg-search text)
        (ivy-quit-and-run
          (color-rg-search-input my-last-swiper-to-counsel-rg-search default-directory))))

    ;; (define-key isearch-mode-map (kbd "<M-return>") 'swiper-toggle-counsel-rg)
    ;; (define-key isearch-mode-map (kbd "<M-return>") 'swiper-toggle-color-rg)
    (define-key ivy-minibuffer-map (kbd "<M-return>") 'swiper-toggle-counsel-rg)
    (define-key ivy-minibuffer-map (kbd "<C-return>") 'swiper-toggle-color-rg)
    )
  )

(defun my-isearch/post-init-swiper ()
  (use-package swiper
    :config

    )
  )



;;; packages.el ends here
