;;; packages.el --- my-org-agenda layer packages file for Spacemacs.
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

(defconst my-org-agenda-packages
  '(
    (org-agenda :location built-in)
    ;; org-super-agenda
    )
  )

(defun my-org-agenda/post-init-org-agenda ()
  (use-package org-agenda
    :config
    ;; 设置为一个目录比较好，这样里面所有的org文件都会加入，如果用第二个，新建的org文件就不会包括
;;; org-agenda-files
    (setq org-directory my-org-gtd-directory)

    ;; (setq org-agenda-files (directory-files-recursively my-org-gtd-directory "\.org$"))
    (setq org-agenda-files '("~/.txnix/.org_notes/gtd/"))
    ;; (setq org-agenda-files '(my-org-gtd-directory))
    ;; (setq org-agenda-files '("~/.txnix/.org_notes/gtd/inbox.org"
    ;;                          "~/.txnix/.org_notes/gtd/gtd.org"
    ;;                          "~/.txnix/.org_notes/gtd/tickler.org"))

;;; org-todo-keywords
    ;; (load-file (expand-file-name "./local/org-agenda-custom.el" (file-name-directory load-file-name)))
    (load-file "/home/dingyi/.txnix/spacemacs/.spacemacs.d/my-org-agenda/local/org-todo-states.el")

    ;; (setq org-todo-keywords
    ;;       '((sequence "TODO(t!)" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)" "|" "DONE(d@/!)" "ABORT(a@/!)")
    ;;         ))

    ;; ;; (setq org-todo-keywords
    ;;       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
    ;;         (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

    ;; ;; (setq org-todo-keyword-faces
    ;;       '(("TODO" :foreground "red" :weight bold)
    ;;         ("NEXT" :foreground "blue" :weight bold)
    ;;         ("DONE" :foreground "forest green" :weight bold)
    ;;         ("WAITING" :foreground "orange" :weight bold)
    ;;         ("HOLD" :foreground "magenta" :weight bold)
    ;;         ("CANCELLED" :foreground "forest green" :weight bold)
    ;;         ("MEETING" :foreground "forest green" :weight bold)
    ;;         ("PHONE" :foreground "forest green" :weight bold)))

;;; org-capture-templates

    (load-file "/home/dingyi/.txnix/spacemacs/.spacemacs.d/my-org-agenda/local/org-catpture-templates.el")
    ;; (setq org-capture-templates
    ;;       '(
    ;;         ("t" "Todo [inbox]" entry
    ;;          (file+headline "~/.txnix/.org_notes/gtd/inbox.org" "Tasks")
    ;;          "* TODO %?\n %U %i\n  %a")
    ;;         ("J" "Journal" entry
    ;;          (file+olp+datetree "~/.txnix/.org_notes/gtd/journal.org")
    ;;          "* %U - %^{heading} %^g\n %?\n")
    ;;         ("T" "Tickler" entry
    ;;          (file+headline "~/.txnix/.org_notes/gtd/tickler.org" "Tickler")
    ;;          "* %i%? \n %U")
    ;;         ))
;;     (setq org-capture-templates
;;       (quote (("t" "todo" entry (file "~/git/org/refile.org")
;;                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("r" "respond" entry (file "~/git/org/refile.org")
;;                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;;               ("n" "note" entry (file "~/git/org/refile.org")
;;                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
;;                "* %?\n%U\n" :clock-in t :clock-resume t)
;;               ("w" "org-protocol" entry (file "~/git/org/refile.org")
;;                "* TODO Review %c\n%U\n" :immediate-finish t)
;;               ("m" "Meeting" entry (file "~/git/org/refile.org")
;;                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;               ("p" "Phone call" entry (file "~/git/org/refile.org")
;;                "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;               ("h" "Habit" entry (file "~/git/org/refile.org")
;;                "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;;; org-refile-targets
    ;; (setq org-refile-targets '(("~/.txnix/.org_notes/gtd/gtd.org" :maxlevel . 3)
    ;;                            ("~/.txnix/.org_notes/gtd/someday.org" :level . 1)
    ;;                            ("~/.txnix/.org_notes/gtd/tickler.org" :maxlevel . 2)))

    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
;;; org-agenda-custom-commands
    (load-file "/home/dingyi/.txnix/spacemacs/.spacemacs.d/my-org-agenda/local/org-agenda-custom-commands.el")
    ;; (setq org-agenda-custom-commands
    ;;       '(("o" "At the office" tags-todo "@office"
    ;;          ((org-agenda-overriding-header "Office")
    ;;           (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

    ;; ;; (defun my-org-agenda-skip-all-siblings-but-first ()
    ;; ;;   "Skip all but the first non-done entry."
    ;; ;;   (let (should-skip-entry)
    ;; ;;     (unless (org-current-is-todo)
    ;; ;;       (setq should-skip-entry t))
    ;; ;;     (save-excursion
    ;; ;;       (while (and (not should-skip-entry) (org-goto-sibling t))
    ;; ;;         (when (org-current-is-todo)
    ;; ;;           (setq should-skip-entry t))))
    ;; ;;     (when should-skip-entry
    ;; ;;       (or (outline-next-heading)
    ;; ;;           (goto-char (point-max))))))
    ;; ;; (defun org-current-is-todo ()
    ;; ;;   (string= "TODO" (org-get-todo-state)))
    ;; (setq my/org-agenda-inbox-view
    ;;       `("i" "Inbox" todo ""
    ;;         ((org-agenda-files '("~/.txnix/.org_notes/gtd/inbox.org")))))

    ;; (add-to-list 'org-agenda-custom-commands `,my/org-agenda-inbox-view)

    ;; (setq my/org-agenda-someday-view
    ;;       `("s" "Someday" todo ""
    ;;         ((org-agenda-files '("~/.txnix/.org_notes/gtd/someday.org")))))

    ;; (add-to-list 'org-agenda-custom-commands `,my/org-agenda-someday-view)

;;; org-agenda-style
    (load-file "/home/dingyi/.txnix/spacemacs/.spacemacs.d/my-org-agenda/local/org-agenda-project.el")
;;; keybindings
    (comma-def "a" 'org-agenda)
    (general-def
      :states '(normal evilified)
      :keymaps 'override
      "M-c" (general-key-dispatch
                (lambda () (interactive) (org-capture nil "tt") (sleep-for 0.1) (evil-insert-state))
              :timeout 0.2
              "M-c" 'org-capture
                )
      "M-a" (general-key-dispatch
                (lambda ()
                  (interactive)
                  ;; (let ((org-agenda-files '("~/.txnix/.org_notes/gtd/")))
                    (org-agenda nil "n")
                    ;; )
                  )
              :timeout 0.2
              ;; "M-a" 'org-agenda
              "M-a" (lambda () (interactive) (org-agenda nil "a"))
              )

      )
    (setq-default org-agenda-files '("~/.txnix/.org_notes/gtd/"))
    )
  )

(defun my-org-agenda/init-org-supre-agenda ()
  (use-package org-super-agenda
    :config
    (org-super-agenda-mode)
    (require 'org-habit)
    (load-file "/home/dingyi/.txnix/spacemacs/.spacemacs.d/my-org-agenda/local/org-super-agenda.el" )
    ))

;;; packages.el ends here
