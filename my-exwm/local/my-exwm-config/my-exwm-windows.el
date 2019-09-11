;;; my-exwm-windows.el --- exwm windows management.  -*- lexical-binding: t; -*-

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

;;;; Moving Windows
(exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
(exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
(exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
(exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)


(provide 'my-exwm-windows)
;;; my-exwm-windows.el ends here

