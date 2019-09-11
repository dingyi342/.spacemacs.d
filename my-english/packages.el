;;; packages.el --- my-english layer packages file for Spacemacs.
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

(defconst my-english-packages
  '(
    (company-translate :location local)
    ;; (company-english-helper :location (recipe :fetcher github
    ;;                                           :repo "manateelazycat/company-english-helper"))
    ;; (company-english-helper :location local)
    ;; (insert-translated-name :location local)
    ;; (insert-translated-name :location (recipe :fetcher github
    ;;                                           :repo "manateelazycat/insert-translated-name"))
    google-translate
    youdao-dictionary
    )
)

(defun my-english/init-company-translate ()
  (use-package company-translate
    :defer (spacemacs/defer)
    :commands (company-google-translate)
    :config
    )
  )

(defun my-english/init-company-english-helper ()
  (use-package company-english-helper
    :defer (spacemacs/defer)
    :config
    ;; (require 'company-english-helper)
    ;; 自己的词典
    ;; (require 'company-english-helper-data)
    (add-hook #'org-mode-hook #'toggle-company-english-helper)
    )
  )

(defun my-english/init-insert-translated-name ()
  "去除对pyim的绑定"
  (use-package insert-translated-name
    :config
    )
  )

(defun my-english/post-init-google-translate ()
  (use-package google-translate
    :config
    (setq google-translate-default-target-language "en")
    (setq google-translate-default-source-language "cn")
    )
  )

(defun my-english/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer (spacemacs/defer)
    :config
    )
  )

;;; packages.el ends here

