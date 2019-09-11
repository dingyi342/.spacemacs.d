;;; dingyi-keybindings.el --- dingyi keybindings.    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <dingyi@dingyi>
;; Keywords: languages, 

;; (defvar spacemacs-comma-leader-map (make-sparse-keymap))

;; (defun spacemacs/set-comma-leader-keys (key def &rest bindings)
;;   (while key
;;     (define-key spacemacs-comma-leader-map (kbd key) def)
;;     (setq key (pop bindings) def (pop bindings))))
;; (put 'spacemacs/set-comma-leader-keys 'lisp-indent-function 'defun)


;; (defvar spacemacs-semicolon-leader-map (make-sparse-keymap))

;; (defun spacemacs/set-semicolon-leader-keys (key def &rest bindings)
;;   (while key
;;     (define-key spacemacs-semicolon-leader-map (kbd key) def)
;;     (setq key (pop bindings) def (pop bindings))))
;; (put 'spacemacs/set-semicolon-leader-keys 'lisp-indent-function 'defun)


;; ;; (defvar spacemacs-r-leader-map (make-sparse-keymap))

;; ;; (defun spacemacs/set-r-leader-keys (key def &rest bindings)
;; ;;   (while key
;; ;;     (define-key spacemacs-r-leader-map (kbd key) def)
;; ;;     (setq key (pop bindings) def (pop bindings))))
;; ;; (put 'spacemacs/set-r-leader-keys 'lisp-indent-function 'defun)

;; ;; (defvar spacemacs-super-space-leader-map (make-sparse-keymap))

;; (defun spacemacs/set-super-space-leader-keys (key def &rest bindings)
;;   (while key
;;     (define-key spacemacs-super-space-leader-map (kbd key) def)
;;     (setq key (pop bindings) def (pop bindings))))
;; (put 'spacemacs/set-super-space-leader-keys 'lisp-indent-function 'defun)


;; (bind-map spacemacs-semicolon-leader-map
;;   :prefix-cmd spacemacs-semicolon-cmds
;;   :keys nil
;;   :evil-keys (";")
;;   :override-minor-modes t
;;   :override-mode-name spacemacs-leader-override-mode)



;; (bind-map spacemacs-super-space-leader-map
;;   :prefix-cmd spacemacs-super-space-cmds
;;   :keys ("s-SPC")
;;   :evil-keys nil
;;   :override-minor-modes t
;;   :override-mode-name spacemacs-leader-override-mode)


(provide 'dingyi-keybindings)
;;; dingyi-keybindings.el ends here
