;;; my-exwm-minibuffer.el --- exwm minibuffer config.  -*- lexical-binding: t; -*-

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

;;;; hide echo area
(defun my-exwm/hide-echo-area ()
  (setq exwm-workspace-minibuffer-position 'bottom)
  (setq exwm-workspace-display-echo-area-timeout 0.2)
  ;; 跟helm不配，用ivy
  )



(provide 'my-exwm-minibuffer)
;;; my-exwm-minibuffer.el ends here

