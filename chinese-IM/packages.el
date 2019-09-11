;;; packages.el --- chinese-IM layer packages file for Spacemacs.
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

(defconst chinese-IM-packages
  '(
    ;; (flypy :location (recipe :fetcher github
    ;;                          :repo "dingyi342/flypy-emacs"))
    (flypy :location local)
    (flypy-en :location local)
    (asim :location local)
    pyim
    )
)

;;; flypy
(defun chinese-IM/init-flypy ()
  (use-package flypy
    ;; :defer (spacemacs/defer)
    ;; :if (eq 'flypy chinese-default-input-method)
    :init
    (progn
      ;; enable flypy on text/org mode
      ;; (add-hook 'text-mode-hook 'activate-default-input-method)
      ;; (add-hook 'org-mode-hook 'activate-default-input-method)
      ;; disable flypy on helm
      ;; (add-hook 'helm-minibuffer-set-up-hook 'deactivate-input-method)
      ;; (add-hook 'helm-before-initialize-hook 'deactivate-input-method)
      ;; (add-hook 'helm-quit-hook 'activate-default-input-method)
      ;; 退出激活默认的输入法
      ;; (add-hook 'helm-quit-hook 'my-inherit-input-method)

      ;; 设置默认输入法为flypy
      ;; (setq default-input-method "chinese-flypy")

      ;; minibuffer激活主buffer的输入法
      ;; (defun my-inherit-input-method ()
      ;;   "Inherit input method from `minibuffer-selected-window'."
      ;;   (let* ((win (minibuffer-selected-window))
      ;;          (buf (and win (window-buffer win))))
      ;;     (when buf
      ;;       (activate-input-method (buffer-local-value 'current-input-method buf)))))

      ;; (add-hook 'minibuffer-setup-hook #'my-inherit-input-method)

      ;; 用minibuffer输入中文
      ;; (add-hook 'minibuffer-setup-hook #'activate-default-input-method)
      ;; (add-hook 'minibuffer-setup-hook #'deactivate-input-method)

      (defun helm-ag-search-flypy ()
        (interactive)
        (helm-aif "/home/dingyi/.txnix/spacemacs/.spacemacs.d/chinese-IM/local/flypy/flypy.txt"
            (helm-do-ag default-directory (list it))
          (error "Error: This buffer is not visited file.")))

      (defun edit-flypy ()
        (interactive)
        (no-confirm
         (find-file "~/.txnix/spacemacs/.spacemacs.d/chinese-IM/local/flypy/flypy.el")
         )
        (read-only-mode -1)
        )

    )))

;;; flypy-en
(defun chinese-IM/init-flypy-en ()
  (use-package flypy-en
    :config
    )
  )
;;; asim
(defun chinese-IM/init-asim ()
  (use-package asim
    :config
    )
  )
;;; pyim
(defun chinese-IM/init-pyim ()
  (use-package pyim
  ;;   :demand t
  ;;   :config
  ;;   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
  ;;   (use-package pyim-basedict
  ;;     :ensure nil
  ;;     :config (pyim-basedict-enable))

  ;;   (setq default-input-method "pyim")

  ;;   ;; 我使用全拼
  ;;   (setq pyim-default-scheme 'quanpin)

  ;;   ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;;   ;; 我自己使用的中英文动态切换规则是：
  ;;   ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;;   ;; 2. 光标前是汉字字符时，才能输入中文。
  ;;   ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  ;;   (setq-default pyim-english-input-switch-functions
  ;;                 '(pyim-probe-dynamic-english
  ;;                   pyim-probe-isearch-mode
  ;;                   pyim-probe-program-mode
  ;;                   pyim-probe-org-structure-template))

  ;;   (setq-default pyim-punctuation-half-width-functions
  ;;                 '(pyim-probe-punctuation-line-beginning
  ;;                   pyim-probe-punctuation-after-punctuation))

  ;;   ;; 开启拼音搜索功能
  ;;   (pyim-isearch-mode 1)

  ;;   ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;;   ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;;   ;; 手动安装 posframe 包。
  ;;   (setq pyim-page-tooltip 'popup)

  ;;   ;; 选词框显示5个候选词
  ;;   (setq pyim-page-length 5)

  ;;   :bind
  ;;   (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
    ;;    ("C-;" . pyim-delete-word-from-personal-buffer))
    )
  )

;;; packages.el ends here
