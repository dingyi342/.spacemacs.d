;;; my-exwm-modeline.el --- exwm modeline configs.   -*- lexical-binding: t; -*-

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
;; Make class name the buffer name.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))


;;; hide modeline line/char-mode
(defun exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (call-interactively #'exwm-input-grab-keyboard)
      ;; (exwm-layout-show-mode-line)
      (exwm-layout-hide-mode-line)
      )))

(defun exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (call-interactively #'exwm-input-release-keyboard)
      (exwm-layout-hide-mode-line))))

(defun exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (cl-second (cl-second mode-line-process)) "line")
          (exwm-input-char-mode)
        (exwm-input-line-mode)))))

(add-hook 'exwm-manage-finish-hook #'exwm-layout-hide-mode-line)
(add-hook 'exwm-manage-finish-hook #'exwm-input-line-mode)


(exwm-input-set-key (kbd "s-z")
                    (lambda () (interactive)
                      (exwm-input-toggle-mode)))



(provide 'my-exwm-modeline)
;;; my-exwm-modeline.el ends here

