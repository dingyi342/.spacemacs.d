;;; packages.el --- my-search layer packages file for Spacemacs.
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

(defconst my-search-packages
  '(
    helm-projectile
    helm-ag
    helm-rg
    (color-rg :location (recipe :fetcher github
                                :repo "manateelazycat/color-rg"))
    ;; (grep-dired :location (recipe :fetcher github
                                  ;; :repo "manateelazycat/grep-dired"))
    )
)

(defun my-search/post-init-helm-projectile ()
  (use-package helm-projectile
    :config
    ;; (defun search-in-current-dir ()
    ;;   (interactive)
    ;;   (let ((default-directory (helm-current-directory)))
    ;;     (spacemacs/helm-project-smart-do-search)))
    (defun my/search ()
      (interactive)
      (if (eq major-mode 'org-mode)
          (helm-projectile-do-search-in-org-directory)
        (if (eq major-mode 'dired-mode)
            (spacemacs/helm-dir-do-ag)
          (helm-swoop)
          )
        )
      )
    ))

(defun my-search/post-init-helm-ag ()
  (use-package helm-ag
    :init
    ;; 使得能搜索隐藏文件
    (setq helm-ag-base-command "ag --hidden --nocolor --nogroup")
    )
)

(defun my-search/init-helm-rg ()
  (use-package helm-rg
    :config
    (setq helm-rg-default-extra-args "--hidden")

    ;; 没有添加 --hidden 参数
    ;; 可以 C-- 或者 M-- 然后 SPC s p 输入参数 --hidden Enter，再搜索
    (defun my/helm-project-do-rg ()
      "Search in current project with `rg'."
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (my/helm-files-do-rg dir)
          (message "error: Not in a project."))))

    (defun my/helm-files-do-rg (&optional dir)
      "Search in files with `rg'."
      (interactive)
      ;; --line-number forces line numbers (disabled by default on windows)
      ;; no --vimgrep because it adds column numbers that wgrep can't handle
      ;; see https://github.com/syl20bnr/spacemacs/pull/8065
      (let* ((root-helm-ag-base-command "rg --hidden --smart-case --no-heading --color never --line-number")
             (helm-ag-base-command (if spacemacs-helm-rg-max-column-number
                                       (concat root-helm-ag-base-command " --max-columns " (number-to-string spacemacs-helm-rg-max-column-number))
                                     root-helm-ag-base-command)))
        (helm-do-ag dir)))

    (spacemacs/set-leader-keys "sp" #'my/helm-project-do-rg)
    ;; (spacemacs/set-leader-keys "sp" #'helm-projectile-rg)
    )
  )

(defun my-search/init-color-rg ()
  (use-package color-rg
    :config
    )
  )

;; 没什么用，首先不支持helm/ivy等模糊搜索匹配，其次把搜索结果形成一个dired bufffer，helm也可以。
;; (defun my-search/init-grep-dired ()
;;   (use-package grep-dired
;;     :config
;;     )
;;   )

;;; packages.el ends here
