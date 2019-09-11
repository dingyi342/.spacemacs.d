;;; my-exwm-desktop.el --- exwm desktop environment.  -*- lexical-binding: t; -*-

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

;;; 调节声音
(defun exwm/amixer+5% ()
  (interactive)
  (call-process-shell-command  "amixer set Master 5%+")
  )

(defun exwm/amixer-5% ()
  (interactive)
  (call-process-shell-command  "amixer set Master 5%-")
  )

(defun exwm/amixer+10% ()
  (interactive)
  (call-process-shell-command  "amixer set Master 10%+")
  )

(defun exwm/amixer-10% ()
  (interactive)
  (call-process-shell-command  "amixer set Master 10%-")
  )

(defun exwm/toggle-mute ()
  (interactive)
  (call-process-shell-command  "amixer set Master toggle")
  )

(exwm-input-set-key (kbd "s-<up>") 'exwm/amixer+5%)
(exwm-input-set-key (kbd "s-<down>") 'exwm/amixer-5%)
;;; 调节亮度

(defun exwm/get-current-bright ()
  (let ((cmd "xrandr --current --verbose | grep -m 1 'Brightness:' | cut -f2- -d:"))
    (string-to-number (shell-command-to-string cmd))
    ))

(defun exwm/bright+ () 
  (interactive)
  (let* (
        (current-brightness (string-to-number (shell-command-to-string  "xrandr --current --verbose | grep -m 1 'Brightness:' | cut -f2- -d:")))
        (set-brightness (+ current-brightness 0.1))
        )
    (if (and
         (not (> set-brightness 1.0))
         (not (< set-brightness 0.0))
         )
        (call-process-shell-command (format "xrandr --output HDMI-1 --brightness %s" set-brightness))
        )
    )
  )
(defun exwm/bright- () 
  (interactive)
  (let* (
         (current-brightness (string-to-number (shell-command-to-string  "xrandr --current --verbose | grep -m 1 'Brightness:' | cut -f2- -d:")))
         (set-brightness (+ current-brightness -0.1))
         )
    (if (and
         (not (> set-brightness 1.0))
         (not (< set-brightness 0.0))
         )
        (call-process-shell-command (format "xrandr --output HDMI-1 --brightness %s" set-brightness))
        )
    )
  )


(exwm-input-set-key (kbd "s-<left>") 'exwm/bright-)
(exwm-input-set-key (kbd "s-<right>") 'exwm/bright+)


(provide 'my-exwm-desktop)
;;; my-exwm-desktop.el ends here

