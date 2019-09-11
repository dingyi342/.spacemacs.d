;;; ffmpeg.el --- ffmepg emacs interface.            -*- lexical-binding: t; -*-

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

;;; 裁减视频
;; ffmpeg -i movie.mp4 -ss 00:00:03 -t 00:00:08 -async 1 cut.mp4

;;; 将 ts 格式转成 mp4 格式
(defun ffmpeg-convertor ()
  (interactive)
  (let* (
        (input (read-file-name "Enter file name: "))
        (choices '("mp4" "mkv" "avi"))
        (output-fromat (ivy-completing-read "output format" choices ))
        (output (concat (file-name-directory input) (read-string "Enter output file name: ") "." output-fromat))
        )
    (if (string-equal output-fromat "mp4")
        (setq ffmpeg-convert-command (format "ffmpeg -i %s -c:v libx264 -c:a aac %s" input output)))
    ;; (async-shell-command ffmpeg-convert-command)
    (message ffmpeg-convert-command)
  ))

;;; 提取视频中的声音
;; ffmpeg -i video.avi -f mp3 audio.mp3

(provide 'ffmpeg)
;;; ffmpeg.el ends here

;; (defun my-pick-one ()
;;   "Prompt user to pick a choice from a list."
;;   (interactive)
;;   (let ((choices '("cat" "dog" "dragon" "tiger")))
;;     (message "%s" (ivy-completing-read "Open bookmark:" choices ))))

;; (defun ask-name (x)
;;   "Ask name."
;;   (interactive "sEnter your name: ")
;;   (message "Name: %s" x))

;; (defun ask-age (x)
;;   "Ask age."
;;   (interactive "nEnter your age: ")
;;   (message "Name: %d" x))

;; (defun print-region-boundary (x y)
;;   "Prints region start and end positions"
;;   (interactive "r")
;;   (message "Region begin at: %d, end at: %d" x y))

;; (defun do-something (x y)
;;   "Ask name and age"
;;   (interactive
;;    ;; complex code here that returns a list
;;    (list "Mary" 22))
;;   (message "Name is: %s, Age is: %d" x y))

;; (defun ask-name-and-age (x y)
;;   "Ask name and age"
;;   (interactive "sEnter you name:
;; nEnter your age: ")
;;   (message "Name is: %s, Age is: %d" x y))

;; (if (y-or-n-p "Do it?")
;;     (progn
;;       ;; code to do something here
;;       )
;;   (progn
;;     ;; code if user answered no.
;;     )
;;   )

