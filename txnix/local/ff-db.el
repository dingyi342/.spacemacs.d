;;; ff-db.el --- find file rely on date base.        -*- lexical-binding: t; -*-

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
(require 'cl)
(require 's)
(require 'emacsql-sqlite)

(defcustom ff-db-root "~/.emacs.d/ff-db/"
  "Root directory for ff-db files"
  :group 'ff-db)

(defcustom ff-db-name "ff-db.sqlite"
  "Name of the sqlite database file."
  :group 'ff-db)

(defcustom ff-db-index-content nil
  "Controls if the content of files is saved."
  :group 'ff-db)

(unless (file-directory-p ff-db-root)
  (make-directory ff-db-root t))

(defvar ff-db (emacsql-sqlite (expand-file-name ff-db-name ff-db-root))
  "Variable for the 'ff-db' connection.")





(provide 'ff-db)
;;; ff-db.el ends here
