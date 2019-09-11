;;; packages.el --- my-evil layer packages file for Spacemacs.
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

(defconst my-evil-packages
  '(
    evil
    evil-collection
    )
)

(defun my-evil/post-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config
    (defun evil-stop-execute-in-insert-state ()
      (when (and (not (eq this-command #'evil-execute-in-insert-state))
                 (not (minibufferp)))
        (remove-hook 'post-command-hook 'evil-stop-execute-in-insert-state)
        (when (buffer-live-p evil-execute-in-insert-state-buffer)
          (with-current-buffer evil-execute-in-insert-state-buffer
            (if (and (eq evil-previous-state 'visual)
                     (not (use-region-p)))
                (progn
                  (evil-change-to-previous-state)
                  (evil-exit-visual-state))
              (evil-change-to-previous-state))))
        (setq evil-execute-in-insert-state-buffer nil)))

    (evil-define-command evil-execute-in-insert-state ()
      "Execute the next command in insert state."
      (add-hook 'post-command-hook #'evil-stop-execute-in-insert-state t)
      (setq evil-execute-in-insert-state-buffer (current-buffer))
      (cond
       ((evil-visual-state-p)
        (let ((mrk (mark))
              (pnt (point)))
          (evil-insert-state)
          (set-mark mrk)
          (goto-char pnt)))
       (t
        (evil-insert-state)))
      (evil-echo "Switched to Emacs state for the next command ..."))
    )
  )

(defun my-evil/init-evil-collection ()
  (let ((inhibit-message nil))
    (use-package evil-collection
      :after evil
      :init
      (defadvice display-warning
          (around no-warn-evil-collection (type message &rest unused) activate)
        "Ignore the warning about set `evil-want-keybinding' to nil"
        (unless (and
                 (> 5 4)
                 ;;(eq type 'evil-collection)  ;; 这个有问题。
                 (or (string-prefix-p "Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details." message t)
                     (string-prefix-p "`evil-want-keybinding' was set to nil but not before loading evil.\
\n
Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details." message t))
                 )
          ad-do-it))
      (ad-activate 'display-warning)
      :config
      (setq evil-collection-mode-list nil)
      )
    )
  )

;;; packages.el ends here
