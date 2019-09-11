;;; my-deepin-screenshot.el --- deepin screenshot    -*- lexical-binding: t; -*-

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

(defvar my-deepin-screenshot-dir "~/OneDrive/org/org-imgs/"
  "where screenshot stores")

(defvar my-deepin-screenshot-timer 2
  "time delay")

(defvar my-deepin-screenshot-org-width 500
  "org image attr width")

(defun my/kill-new-img-link(prefix imagename)
  (kill-new (concat
             (format "#+attr_org: :width %dpx\n" my-deepin-screenshot-org-width)
             (format "[[file:%s%s]] " prefix imagename)
             )))

;;;###autoload
(defun my/gen-file-name(&optional basename)
  (interactive)
  (unless (and basename (not (string= basename "")))
    (setq basename (file-name-base buffer-file-name)))
  (concat basename (format-time-string "_%Y%H%M%S")))


;;;###autoload
(defun my-deepin-screenshot(basename &optional filename)
  (interactive "sScreenshot name: ")
  ;; 跟据基础名加上年月日形成文件名,有效避免重名
  (setq filename (my/gen-file-name basename))
  ;; 加个延时，可以避免在截图上出先多余的线, 同时也可以利用这段时间来切换窗口，buffer等
  (run-with-idle-timer my-deepin-screenshot-timer nil
                       (lambda (filename)
                         (progn
                           (call-process-shell-command (concat "deepin-screenshot -s " (concat (expand-file-name my-deepin-screenshot-dir) filename)))
                           ;; 复制到剪贴板
                           (my/kill-new-img-link my-deepin-screenshot-dir (concat filename ".png")))) filename))

;;;###autoload
(defun my-org-deepin-screenshot(basename &optional filename)
  (interactive "sScreenshot name: ")
  ;; 跟据基础名加上年月日形成文件名,有效避免重名
  (setq filename (my/gen-file-name basename))
  ;; 加个延时，可以避免在截图上出先多余的线, 同时也可以利用这段时间来切换窗口，buffer等
  (run-with-idle-timer my-deepin-screenshot-timer nil
                       (lambda (filename)
                         (progn
                           (call-process-shell-command (concat "deepin-screenshot -s " (concat (expand-file-name my-deepin-screenshot-dir) filename)))
                           ;; 复制到剪贴板
                           (my/kill-new-img-link my-deepin-screenshot-dir (concat filename ".png")))) filename)
  (with-current-buffer (current-buffer)
    (insert (format "#+attr_org: :width %dpx\n" my-deepin-screenshot-org-width))
    (insert "\n")
    (insert (format "[[file:%s%s]] " my-deepin-screenshot-dir (concat filename ".png")))
    (run-with-idle-timer 1.5 nil #'org-redisplay-inline-images)
    )
  )

(provide 'my-deepin-screenshot)
;;; my-deepin-screenshot.el ends here
