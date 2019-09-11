;; (require 'awesome-tray) 
;; (awesome-tray-mode 1)
;; 启用了awesome-tray-mode,自动隐藏 mode-line-mode

;; (set (make-variable-buffer-local 'mode-line-format) nil)
;; (add-hook 'window-configuration-change-hook
;;          (lambda ()
;;            (with-current-buffer (current-buffer)
;;              (if awesome-tray-active-p
;;                  (spacemacs/toggle-mode-line-off))
;;              )
;;            ))

;; ;;;; https://www.emacswiki.org/emacs/HideModeLine
;; (if awesome-tray-active-p
;; (setq-default mode-line-format nil))
;; (setq mode-line-format nil)
;; (setq-default mode-line-format nil)

;; (add-hook 'awesome-tray-mode-hook 'spacemacs/toggle-mode-line-off)
;; 不要对modeline修改，用spacemacs的 toggle-mode-line来让用户自己选择，默认是不显示。

;; 修复M-x输入的字符不可见。

