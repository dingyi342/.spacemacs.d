;;; packages.el --- my-org-db layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: dingyi <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst my-org-db-packages
  '(
    ;; (emacsql :location (recipe :fetcher github
    ;;                            :repo "skeeto/emacsql"
    ;;                            :files ("emacsql.el")
    ;;                            ))
    ;; (emacsql-sqlite :location (recipe :fetcher github
    ;;                                   :repo "skeeto/emacsql"
    ;;                                   :files ("emacsql-sqlite.el")))
    ;; (org-db :location (recipe :fetcher github
    ;;                           :repo "jkitchin/scimax"
    ;;                           :files ("org-db.el")
    ;;                           ))
    ;; org-sql
    emacsql
    emacsql-sqlite
    (org-db :location local)
    )
)

(defun my-org-db/init-emacsql ()
  ;;https://github.com/skeeto/emacsql
  ;; 用elisp与支持SQL数据库沟通
  (require 'emacsql)
  ;; (use-package emacsql)
  )

(defun my-org-db/init-emacsql-sqlite ()
  ;; (use-package emacsql-sqlite)
  (require 'emacsql-sqlite)
  )

(defun my-org-db/init-org-db ()
  (use-package org-db
    ;; :defer (spacemacs/defer)
    :config
    (setq
     org-db-root (concat spacemacs-cache-directory "org-db/")
     ;; org-db-rooot "~/org-db/"
     ;; 这样加的话，就会一直更新，还是不加的为好。
     ;; 默认的是打开的org文件，才会添加到 org-db-queue。
     ;; org-db-queue (directory-files-recursively "~/.txnix/.org_notes/notes/" "\.org$")
     )

    (defun ivy/org-db-open-heading ()
      (interactive)
      (ivy-read
       "heading: "
       (org-db-heading-candidates)
       :action
       (lambda (x)
         ;; (message "value: %s" x)
         (find-file (lax-plist-get (cdr x) :file))
         (goto-char (lax-plist-get (cdr x) :begin))
         (org-show-entry)
         )
       ;; :multi-action
       ;; (lambda (x)
       ;;   (switch-to-buffer (get-buffer-create "*org-db*"))
       ;;   (org-mode)
       ;;   (loop for hl in x do
       ;;         (save-excursion
       ;;           (with-current-buffer (find-file-noselect (lax-plist-get (cdr hl) :file))
       ;;             (goto-char (lax-plist-get (cdr hl) :begin))
       ;;             (org-copy-subtree)))
       ;;         (org-yank)
       ;;         (insert "\n")))
       :caller 'ivy/org-db-open-heading
       ))

    (ivy-set-actions
     'ivy/org-db-open-heading
     `(("c" ,(lambda (_)
               (switch-to-buffer (get-buffer-create "*org-db*"))
               (org-mode)
               (loop for hl in ivy-marked-candidates do
                     (save-excursion
                       (with-current-buffer (find-file-noselect (plist-get hl) :file)
                         (goto-char (plist-get hl) :begin)
                         (org-copy-subtree)))
                     (org-yank)
                     (insert "\n"))) "colletc")
       ("s" ,(lambda (_)
               "This saves the marked candidates so you could use them in another code."
               (setq org-db-marked-candidates ivy-marked-candidates)) "save")))

    ;; (ivy-read "Contact: " (org-db-contacts-candidates))

    ;; :general ;; 加这个了延迟加载就不执行了。
    ;; (comma-def "j" 'org-db-open-heading)
    (comma-def "j" 'ivy/org-db-open-heading)
    )
  )

;; (defun my-org-db/init-org-sql ()
;;   (use-package org-sql
;;     :config
;;     (setq org-sqlite-db-path (concat spacemacs-cache-directory "org-sql")
;;           org-sql-files (directory-files-recursively my-org-notes-directory "\.org$")
;;           )
;;     )
  ;; )
;;; packages.el ends here
