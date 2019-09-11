;;; packages.el --- my-describe layer packages file for Spacemacs.
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
;; https://www.emacswiki.org/emacs/HelpPlus
;;; Code:

(defconst my-describe-packages
  '(
    emaps
    (help-fns+ :location local)
    helpful
    elisp-demos
    )
)

(defun my-describe/init-emaps ()
  (use-package emaps
    :commands (emaps-describe-keymap-bindings
               emaps-describe-keymap)
    :config

    :general
    (general-def
      "C-h K" 'emaps-describe-keymap-bindings
      "C-h C-k" 'emaps-describe-keymap-bindings
      )
    )
  )

;; `C-h B'      `describe-buffer'
;;    `C-h c'      `describe-command'     (replaces `describe-key-briefly')
;;    `C-h o'      `describe-option'
;;    `C-h C-c'    `describe-key-briefly' (replaces `C-h c')
;;    `C-h C-o'    `describe-option-of-type'
;;    `C-h M-c'    `describe-copying'     (replaces `C-h C-c')
;;    `C-h M-f'    `describe-file'
;;    `C-h M-k'    `describe-keymap'
;;    `C-h M-l'    `find-function-on-key'
;; spacemas已经自带了啊。
;; SPC h d 不全
(defun my-describe/post-init-help-fns+ ()
  (use-package help-fns+
    :config
    (spacemacs/set-leader-keys
      "hdB" 'describe-Buffer
      )
    )
  )

(defun my-describe/init-helpful ()
  (use-package helpful
    :config
    ;; Note that the built-in `describe-function' includes both functions
    ;; and macros. `helpful-function' is functions only, so we provide
    ;; `helpful-callable' as a drop-in replacement.
    (global-set-key (kbd "C-h f") #'helpful-callable)

    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)

    ;; Lookup the current symbol at point. C-c C-d is a common keybinding
    ;; for this in lisp modes.
    (global-set-key (kbd "C-c C-d") #'helpful-at-point)

    ;; Look up *F*unctions (excludes macros).
    ;;
    ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
    ;; already links to the manual, if a function is referenced there.
    (global-set-key (kbd "C-h F") #'helpful-function)

    ;; Look up *C*ommands.
    ;;
    ;; By default, C-h C is bound to describe `describe-coding-system'. I
    ;; don't find this very useful, but it's frequently useful to only
    ;; look at interactive functions.
    (global-set-key (kbd "C-h C") #'helpful-command)

    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)
    )
  )

(defun my-describe/init-elisp-demos ()
  (use-package elisp-demos
    :config
    (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    ;; helpful intergrate
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
    )
  )
;;; packages.el ends here
