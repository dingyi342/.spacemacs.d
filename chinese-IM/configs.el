;;; configs.el --- chinese input method config.      -*- lexical-binding: t; -*-

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

;;; 强制输入英文标点
(general-def
  :keymaps 'override
  :states '(insert emacs)
  "," (lambda () (interactive) (insert ","))
  ";" (lambda () (interactive) (insert ";"))
  "." (lambda () (interactive) (insert "."))
  "<" (lambda () (interactive) (insert "<"))
  ">" (lambda () (interactive) (insert ">"))
  "?" (lambda () (interactive) (insert "?"))
  "'" (lambda () (interactive) (insert "'"))
  )

(provide 'configs)
;;; configs.el ends here
