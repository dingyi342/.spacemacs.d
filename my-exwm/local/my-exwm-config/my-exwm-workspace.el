;;; my-exwm-workspace.el --- exwm workspace config.  -*- lexical-binding: t; -*-

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

(unless (get 'exwm-workspace-number 'saved-value)
  (setq exwm-workspace-number 10))

(setq exwm-workspace-number 10
      exwm-workspace-current-index 1
      exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t
      exwm-input-line-mode-passthrough nil
      )

(provide 'my-exwm-workspace)
;;; my-exwm-workspace.el ends here

