;;; keybindings.el --- my exwm default keybindings   -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <dingyi@dingyi>
;; Keywords:

(defun exwm-input--set-key (key command)
  (exwm--log "key: %s, command: %s" key command)
  (global-set-key key command)
  (cl-pushnew key exwm-input--global-keys))


(defun exwm-input-set-key (key command)
  "Set a global key binding.

The new key binding only takes effect in real time when this command is
called interactively, and is lost when this session ends unless it's
specifically saved in the Customize interface for `exwm-input-global-keys'.

In configuration you should customize or set `exwm-input-global-keys'
instead."
  (interactive "KSet key globally: \nCSet key %s to command: ")
  (exwm--log)
  (setq exwm-input-global-keys (append exwm-input-global-keys
                                       (list (cons key command))))
  (exwm-input--set-key key command)
  (when (called-interactively-p 'any)
    (exwm-input--update-global-prefix-keys)))




(defun exwm-input-set-key* (key command)
  "This function is similar with `exwm-input-set-key', the
different is that `exwmx-input-set-key' protect `key' from
being override by other minor modes with the help of `bind-key*'."
  (exwm-input-set-key key command)
  (bind-key* key command))

(defun exwm-input-set-keys (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(defun exwm-input-set-keys* (key command &rest bindings)
  (while key
    (exwm-input-set-key* (kbd key)
                        ;; `(lambda ()
                           ;; (interactive)
                           ;; (start-process-shell-command ,command nil ,command))
                         command
                        )
    (setq key     (pop bindings)
          command (pop bindings))))


(define-prefix-command 'exwm-app-map)


(exwm-input-set-keys*
 "s-a c" 'my/exwm-switch-chromium
 "s-a f" 'my/exwm-switch-firefox
 "s-a e" 'my/exwm-switch-dolphin
 "s-a a" 'my/exwm-switch-anki
 "s-a m" nil

 "s-b" 'exwmx-switch-application
 ;; "s-b" 'helm-exwm

 "s-c" 'my/copy-to-today-file
 ;; "s-c" 'my/exwm-copy

 "s-d" 'bury-buffer
 "s-e" nil
 "s-f" 'exwm-layout-toggle-fullscreen
 "s-g" 'quit-everything
 "s-h" 'evil-window-left
 "s-i" 'exwmx-sendstring-from-minibuffer
 "s-j" 'evil-window-down
 "s-k" 'evil-window-up
 "s-l" 'evil-window-right
 "s-m" 'spacemacs/maximize-vertically
 "s-n" nil
 "s-o" 'counsel-linux-app
 "s-p" 'helm-run-external-command
 "s-q" nil
 "s-r" 'exwm-reset
 "s-s" nil
 "s-t" 'spacemacs/toggle-current-window-dedication
 "s-u" 'winner-undo
 "s-v" 'my/exwm-paste
 "s-w" 'exwm-workspace-switch

 "s-x e" 'spacemacs/shell-pop-eshell
 ;; "s-x f"
 ;; "s-x p" 'my/project-fuzzy-search
 ;; "s-x P" 'my/all-package



 "s-y" nil
 "s-z" 'exwm-input-toggle-mode
 "s-A" nil
 "s-B" nil
 "s-C" 'spacemacs/kill-this-buffer
 "s-D" nil
 "s-E" nil
 "s-F" nil
 "s-G" nil
 "s-H" 'evil-window-move-far-left
 "s-I" 'my/exwmx-sendstring
 "s-J" 'evil-window-move-very-bottom
 "s-K" 'evil-window-move-very-top
 "s-L" 'evil-window-move-far-right
 "s-M" 'spacemacs/maximize-horizontally
 "s-N" nil
 "s-O" nil
 "s-P" nil
 "s-Q" nil
 "s-R" nil
 "s-S" nil
 "s-T" 'spacemacs/toggle-current-window-dedication
 "s-U" 'winner-redo
 "s-V" nil
 "s-W" nil
 "s-X" nil
 "s-Y" nil
 "s-Z" nil

 "s-1" (lambda () (interactive) (exwm-workspace-switch 0))
 "s-2" (lambda () (interactive) (exwm-workspace-switch 1))
 "s-3" (lambda () (interactive) (exwm-workspace-switch 2))
 "s-4" (lambda () (interactive) (exwm-workspace-switch 3))
 "s-5" (lambda () (interactive) (exwm-workspace-switch 4))
 "s-6" (lambda () (interactive) (exwm-workspace-switch 5))
 "s-7" (lambda () (interactive) (exwm-workspace-switch 6))
 "s-8" (lambda () (interactive) (exwm-workspace-switch 7))
 "s-9" (lambda () (interactive) (exwm-workspace-switch 8))
 "s-0" (lambda () (interactive) (exwm-workspace-switch 9))
 )
