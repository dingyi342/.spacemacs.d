;;; packages.el --- my-bookmarks layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst my-bookmarks-packages
  '(
    simple-bookmarks
    autobookmarks                       ;like recentf
    )
  )

(defun my-bookmarks/init-simple-bookmarks ()
  (use-package simple-bookmarks
    :config
    (setq simple-bookmarks-file (concat spacemacs-cache-directory "simple-bookmarks"))
    (simple-bookmarks-init)
    ;; ;; handles all bookmarks
    ;; (global-set-key (kbd "M-- l") 'simple-bookmarks-interactive-execute-from-all)
    ;; (global-set-key (kbd "M-- L") 'simple-bookmarks-interactive-execute)
    ;; (global-set-key (kbd "M-- c") 'simple-bookmarks-interactive-add)
    ;; (global-set-key (kbd "M-- r") 'simple-bookmarks-interactive-remove-from-all)
    ;; (global-set-key (kbd "M-- R") 'simple-bookmarks-interactive-remove)

    ;; ;; handles only file-bookmarks
    ;; (global-set-key (kbd "M-- f l") 'simple-bookmarks-interactive-execute-file)
    ;; (global-set-key (kbd "M-- f c") 'simple-bookmarks-interactive-add-file)
    ;; (global-set-key (kbd "M-- f r") 'simple-bookmarks-interactive-remove-file)

    ;; ;; handles only directory-bookmarks
    ;; (global-set-key (kbd "M-- d l") 'simple-bookmarks-interactive-execute-directory)
    ;; (global-set-key (kbd "M-- d c") 'simple-bookmarks-interactive-add-directory)
    ;; (global-set-key (kbd "M-- d r") 'simple-bookmarks-interactive-remove-directory)

    ;; ;; handles only desktop-bookmarks
    ;; (global-set-key (kbd "M-- s l") 'simple-bookmarks-interactive-execute-desktop)
    ;; (global-set-key (kbd "M-- s c") 'simple-bookmarks-interactive-create-desktop)
    ;; (global-set-key (kbd "M-- s a") 'simple-bookmarks-interactive-add-desktop)
    ;; (global-set-key (kbd "M-- s r") 'simple-bookmarks-interactive-remove-desktop)

    ;; ;; handles only url-bookmarks
    ;; (global-set-key (kbd "M-- u l") 'simple-bookmarks-interactive-execute-url)
    ;; (global-set-key (kbd "M-- u c") 'simple-bookmarks-interactive-add-url)
    ;; (global-set-key (kbd "M-- u r") 'simple-bookmarks-interactive-remove-url)
    )
  )

(defun my-bookmarks/init-autobookmarks ()
  (use-package autobookmarks
    :config
    (autobookmarks-mode)
    (defun counsel-abm-recent ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "abm-recent: " (mapcar #'substring-no-properties (mapcar 'bookmark-name-from-full-record abm-recent-buffers))
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))

    (defun counsel-abm-visited ()
      "Find a file on `recentf-list'."
      (interactive)
      (require 'recentf)
      (recentf-mode)
      (ivy-read "amb-visited: " (mapcar #'substring-no-properties (mapcar 'bookmark-name-from-full-record abm-visited-buffers))
                :action (lambda (f)
                          (with-ivy-window
                            (find-file f)))
                :require-match t
                :caller 'counsel-recentf))
    :general
    (general-def "C-c r" 'counsel-abm-recent
                 "C-c v" 'counsel-abm-visited)
    (general-def
      :states '(normal evilified)
      "M-r" 'counsel-abm-recent
      "M-v" 'counsel-abm-visited
      )
    )
  )

;;; packages.el ends here
