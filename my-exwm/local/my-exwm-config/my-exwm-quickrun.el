;;; my-exwm-quickrun.el --- exwm run or raise.       -*- lexical-binding: t; -*-

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

(require 'exwmx-quickrun)

(defun my/exwmx-quickrun (command &optional search-alias ruler)
  "Run `command' to launch an application, if application's window is found,
just switch to this window, when `search-alias' is t, `command' will be regard
as an appconfig alias and search it from `exwmx-appconfig-file', by default,
:class and :instance is used to search application, user can override
it by argument `ruler', ruler can be a plist with keys: :class, :instance
and :title or just a key list."
  ;;(exwmx--switch-window)
  (let* ((ruler-plist-p (and ruler (exwmx--plist-p ruler)))
         (keys
          ;; Deal with ruler which is like (:class :instance :title)
          (if (and ruler (listp ruler) (not ruler-plist-p))
              ;; (exwmx--clean-keylist ruler)
            '(:class :instance)))
         (appconfigs (exwmx-appconfig--get-all-appconfigs))
         (cmd (if search-alias
                  (or (plist-get (exwmx-appconfig--search
                                  `((:alias ,command)))
                                 :command)
                      (when appconfigs
                        (let ((appconfig (exwmx-appconfig--select-appconfig)))
                          (plist-put appconfig :alias command)
                          (exwmx-appconfig--add-appconfig appconfig)
                          (plist-get appconfig :command))))
                command))
         (buffer (or (if search-alias
                         (exwmx-quickrun--find-buffer
                          (if ruler-plist-p
                              ruler
                            (exwmx-appconfig--get-subset
                             (exwmx-appconfig--search
                              `((:alias ,command)))
                             keys)))
                       (exwmx-quickrun--find-buffer
                        (if ruler-plist-p
                            ruler
                          (exwmx-appconfig--get-subset
                           (exwmx-appconfig--search
                            `((:command ,command)))
                           keys))))
                     ;; The below two rules are just guess rules :-)
                     ;; Suggest use `exwmx-appconfig' to manage app's information.
                     (exwmx-quickrun--find-buffer
                      `(:class ,(capitalize (concat "^" (car (split-string command " "))))))
                     (exwmx-quickrun--find-buffer
                      `(:class ,(concat "^" (car (split-string command " "))))))))
    (if (and search-alias (not cmd))
        (message "EXWM-X: please run `exwmx-appconfig' to add appconfig.")
      (message "EXWM-X Quick Run: %s" cmd))
    ;; If current application window is a floating-window, minumize it.
    (when (and (eq major-mode 'exwm-mode)
               exwm--floating-frame)
      (exwm-floating-hide))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (when cmd
        (exwmx-shell-command cmd)))))



;; (defvar s-a-map (make-sparse-keymap))
(push ?\s-a exwm-input-prefix-keys)

(exwm-input-set-key (kbd "s-a f")
                    (lambda ()
                      (interactive)
                      (my/exwmx-quickrun "firefox" t)
                      )
                    )
(exwm-input-set-key (kbd "s-a v")
                    (lambda ()
                      (interactive)
                      (my/exwmx-quickrun "vlc")
                      )
                    )
(exwm-input-set-key (kbd "s-a d")
                    (lambda ()
                      (interactive)
                      (my/exwmx-quickrun "jetbrains-idea")
                      )
                    )




(provide 'my-exwm-quickrun)
;;; my-exwm-quickrun.el ends here

