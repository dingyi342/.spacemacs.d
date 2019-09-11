;;; my-exwm-input.el --- exwm input.                 -*- lexical-binding: t; -*-

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

(defun my/exwmx-sendstring ()
  "Pop up a buffer and let user input, edit and send string to application."
  (interactive)
  (let ((buffer (get-buffer-create exwmx-sendstring-buffer)))
    (with-current-buffer buffer
      (markdown-mode)
      (exwmx-sendstring-mode)
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\<exwmx-sendstring-mode-map>"
              "Sendstring: "
              "Finish with `\\[exwmx-sendstring-finish]', "
              "Ignore with `\\[exwmx-sendstring-ignore]'. "))))
    (pop-to-buffer buffer)))

(defun dingyi/exwmx-sendstring-with-delay--send (string)
  "Send `string' to clipboard and then send paste key to
    application to trigger paste operation, `string' will be
    inserted into the application.添加延时，否则在chromium里无效"
  (if (derived-mode-p 'exwm-mode)
      (let ((paste-key
             (or (plist-get (exwmx-appconfig--search
                             `((:class ,exwm-class-name)
                               (:instance ,exwm-instance-name)))
                            :paste-key)
                 exwmx-sendstring-default-paste-key)))
        (kill-new string)
        (dolist (key (string-to-list (kbd paste-key)))
          (sleep-for 0.2)
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))

(defun dingyi/exwmx-sendstring--send (delay string)
  "Send `string' to clipboard and then send paste key to
    application to trigger paste operation, `string' will be
    inserted into the application."
  (if (derived-mode-p 'exwm-mode)
      (let ((paste-key
             (or (plist-get (exwmx-appconfig--search
                             `((:class ,exwm-class-name)
                               (:instance ,exwm-instance-name)))
                            :paste-key)
                 exwmx-sendstring-default-paste-key)))
        (kill-new string)
        (dolist (key (string-to-list (kbd paste-key)))
          (sleep-for delay)
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))



(defun dingyi/exwmx-sendstring-finish ()
  "Send the string in buffer and delete window."
  (interactive)
  (if exwmx-sendstring-mode
      (let ((string (buffer-string)))
        (quit-window t)
        ;; (kill-buffer exwmx-sendstring-buffer)
        (exwmx-sendstring--send string))
    (message "EXWM-X: exwmx-sendstring-mode is not enabled.")))

(defun dingyi/exwmx-sendstring-ignore ()
  "Ignore send string to application."
  (interactive)
  (if exwmx-sendstring-mode
      (progn
        (quit-window t)
        ;; (kill-buffer exwmx-sendstring-buffer))
        (message "EXWM-X: exwmx-sendstring-mode is not enabled."))))


(exwmx-input-set-key (kbd "s-i") 'exwmx-sendstring-from-minibuffer)
(exwmx-input-set-key (kbd "s-I") 'my/exwmx-sendstring)
(exwmx-input-set-key (kbd "s-<delete>") 'exwm-logout)

(define-key exwmx-sendstring-mode-map (kbd "C-c '") 'dingyi/exwmx-sendstring-finish)
(define-key exwmx-sendstring-mode-map (kbd "C-c C-c") 'dingyi/exwmx-sendstring-finish)
(define-key exwmx-sendstring-mode-map (kbd "C-c C-k") 'dingyi/exwmx-sendstring-ignore)
;; (define-key exwmx-sendstring-mode-map (kbd "C-c C-q") 'dingyi/exwmx-sendstring-ignore)

;; 没有效果啊。
(spacemacs/set-leader-keys-for-minor-mode 'exwmx-sendstring-mode
  "m" 'dingyi/exwmx-sendstring-finish
  "k" 'dingyi/exwmx-sendstring-ignore
  "a" 'dingyi/exwmx-sendstring-ignore
  )

;; 加了0.2s延时，使得能在chrome上用。
(advice-add 'exwmx-sendstring--send :override #'dingyi/exwmx-sendstring-with-delay--send)


(provide 'my-exwm-input)
;;; my-exwm-input.el ends here

