;;; funcs.el --- my txnix funcs.                     -*- lexical-binding: t; -*-

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

;; add my txnix funcs.

;;; Code:

(defun txnix-find-file ()
  (interactive)
  (let* ((txnix-root-directory "~/.txnix/")
         (my-org-directory "~/.txnix/.org_notes/notes/")
         (file (completing-read "txnix find file: "
                                 (append
                                  (mapcar
                                   (lambda (x)
                                     (expand-file-name x txnix-root-directory)
                                     )
                                   (projectile-project-files txnix-root-directory)
                                   )
                                  (mapcar
                                   (lambda (x)
                                     (expand-file-name x my-org-directory)
                                     )
                                   (projectile-project-files my-org-directory)
                                   )
                                  )
                                 )))
         (find-file file)
         )
    )

;; (defun txnix/find-file-in-directories (dir &rest )
  ;; )

(defcustom txnix-data-dirs '("~/.txnix/" "~/.txnix/.org_notes/notes/")
  "用于txnix-all-files来获取所有目录里的文件")

(defun txnix-all-files ()
  "Get a list of all files in all projects."
  (cl-mapcan
   (lambda (dirs)
     (when (file-exists-p dirs)
       (mapcar (lambda (file)
                 (expand-file-name file dirs))
               (projectile-project-files dirs))))
   txnix-data-dirs))

(defun txnix-find-file-in-dirs ()
  "Jump to a file in any of the known projects."
  (interactive)
  (let* ((file (completing-read "find file in dirs: " (txnix-all-files)))
        (helm-ff-fuzzy-matching nil)
        (helm-use-fuzzy nil)
        (helm-mode-fuzzy-match nil)
        (helm-completion-in-region-fuzzy-match nil)
        (helm-candidate-number-limit 20)
        )
    (find-file file)
    ;; (find-file (projectile-completing-read "txnix find file in dirs: " (txnix-all-files)))
    )
  )

(provide 'funcs)
;;; funcs.el ends here
