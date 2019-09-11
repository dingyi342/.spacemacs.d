;;; my-exwm-keybindings.el --- my exwm keybindings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <dingyi@dingyi>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;; Line-editing shortcuts
(unless (get 'exwm-input-simulation-keys 'saved-value)
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete]))))

;;; Global keybindings.
(unless (get 'exwm-input-global-keys 'saved-value)
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)
          ;; 's-w': Switch workspace.
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-&': Launch application.
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9)))))


;;; 全局默认的exwm keybinding.
(exwm-input-set-keys*
 ;; "s-a c" 'my/exwm-switch-chromium
 ;; "s-a f" 'my/exwm-switch-firefox
 ;; "s-a e" 'my/exwm-switch-dolphin
 ;; "s-a a" 'my/exwm-switch-anki
 ;; "s-a m" nil

 "s-b" 'exwmx-switch-application
 ;; "s-b" 'helm-exwm

 "s-c" 'my/copy-to-today-file
 ;; "s-c" 'my/exwm-copy

 "s-d" 'bury-buffer
 "s-e" nil
 "s-f" 'exwm-layout-toggle-fullscreen
 "s-g" 'my-exwm-default-layout
 "s-h" 'evil-window-left
 "s-i" 'exwmx-sendstring-from-minibuffer
 "s-j" 'evil-window-down
 "s-k" 'evil-window-up
 "s-l" 'evil-window-right
 ;; "s-m" 'spacemacs/maximize-vertically
 "s-n" nil
 "s-o" 'counsel-linux-app
 ;; "s-p" 'helm-run-external-command
 "s-q" nil
 "s-r" 'exwm-reset
 "s-s" nil
 ;; "s-t" 'spacemacs/toggle-current-window-dedication
 "s-u" 'winner-undo
 "s-v" 'my/exwm-paste
 ;; "s-w" 'exwm-workspace-switch
 "s-w" 'exwm-workspace-move-window

 ;; "s-x e" 'spacemacs/shell-pop-eshell
 ;; "s-x f"
 ;; "s-x p" 'my/project-fuzzy-search
 ;; "s-x P" 'my/all-package




 "s-y" nil
 ;; "s-z" 'exwm-input-toggle-mode
 "s-A" nil
 "s-B" nil
 ;; "s-C" 'spacemacs/kill-this-buffer
 "S-c" 'kill-current-buffer
 "s-D" nil
 "s-E" nil
 "s-F" nil
 "s-G" nil
 "s-H" 'evil-window-move-far-left
 "s-I" 'my/exwmx-sendstring
 "s-J" 'evil-window-move-very-bottom
 "s-K" 'evil-window-move-very-top
 "s-L" 'evil-window-move-far-right
 ;; "s-M" 'spacemacs/maximize-horizontally
 "s-N" nil
 "s-O" nil
 "s-P" nil
 "s-Q" nil
 "s-R" nil
 "s-S" nil
 ;; "s-T" 'spacemacs/toggle-current-window-dedication
 "s-U" 'winner-redo
 "s-V" nil
 "s-W" nil
 "s-X" nil
 "s-Y" nil
 "s-Z" nil

 ;; "s-1" (lambda () (interactive) (exwm-workspace-switch 0))
 ;; "s-2" (lambda () (interactive) (exwm-workspace-switch 1))
 ;; "s-3" (lambda () (interactive) (exwm-workspace-switch 2))
 ;; "s-4" (lambda () (interactive) (exwm-workspace-switch 3))
 ;; "s-5" (lambda () (interactive) (exwm-workspace-switch 4))
 ;; "s-6" (lambda () (interactive) (exwm-workspace-switch 5))
 ;; "s-7" (lambda () (interactive) (exwm-workspace-switch 6))
 ;; "s-8" (lambda () (interactive) (exwm-workspace-switch 7))
 ;; "s-9" (lambda () (interactive) (exwm-workspace-switch 8))
 ;; "s-0" (lambda () (interactive) (exwm-workspace-switch 9))

 ;; ;; "<f12>" (lambda () (interactive) (exwmx-quickrun "urxvt"))
 )



(provide 'my-exwm-keybindings)
;;; my-exwm-keybindings.el ends here

