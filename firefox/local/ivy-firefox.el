;;; ivy-firefox.el --- ivy firefox bookmarks.        -*- lexical-binding: t; -*-

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

;;; use `ivy' to use w3m-bookmarks
(require 'dom)
;;; where your bookmark file located
;; (setq peng-bookmark-file "~/.w3m/bookmark.html")
(setq peng-bookmark-file
  (car (file-expand-wildcards "~/.mozilla/firefox/*/bookmarks.html")))


(defun peng-ivy-get-bookmarks ()
  "use `ivy-read' to select bookmark and return its url
"
  (interactive)
  (let* ((dom (with-temp-buffer
                (insert-file-contents peng-bookmark-file)
                (libxml-parse-html-region (point-min)
                                          (point-max))))
         (a (dom-by-tag dom 'a))
         (mylist (mapcar 'peng-parse-dom-tag-to-personal-format a))
         (name (ivy-read "Jump to bookmarks:" (mapcar 'car mylist)))
         ;; todo: I need to find `name' in mylist, if find, break the
         ;; loop. But now, I just use `mapcar' to deal with every
         ;; element in mylist and get `final-result' by removing all
         ;; nil element in the list `result'. It's very ugly code!
         (result (mapcar #'(lambda (data)
                             (if (string= (car data)
                                          name)
                                 (cdr data)
                               nil))
                         mylist))
         (final-result (caar (remove nil result))))
    final-result))


(defun peng-parse-dom-tag-to-personal-format (dom)
  "parse a tag to `(name url) format'"
  (let ((url (dom-attr dom 'href))
        (name (dom-text dom)))
    (list name url)))

;;;###autoload
(defun peng-bookmarks-firefox ()
  (interactive)
  (let ((url (peng-ivy-get-bookmarks)))
    (browse-url-firefox url)))

;;;###autoload
(defun peng-bookmarks-chrome ()
  (interactive)
  (let ((url (peng-ivy-get-bookmarks)))
    (browse-url-chromium url)))

;;;###autoload
(defun peng-bookmarks-w3m ()
  (interactive)
  (let ((url (peng-ivy-get-bookmarks)))
    (w3m-goto-url-new-session url)))


(provide 'ivy-firefox)
;;; ivy-firefox.el ends here
