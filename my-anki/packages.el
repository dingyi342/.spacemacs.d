;;; packages.el --- my-anki layer packages file for Spacemacs.
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

(defconst my-anki-packages
  '(
    anki-editor
    )
)

(defun my-anki/init-anki-editor ()
  (use-package anki-editor
    :config
    (setq anki-editor-create-decks t)

    (general-def 'my-anki-editor-mode-map
      "M-RET" 'my/anki-editor-insert-note
      "C-c C-c" 'anki-editor-push-notes
      "C-c C-r" 'anki-editor-retry-failure-notes
      )

    (define-minor-mode my-anki-editor-mode
      "acvivate keybinding for anki-editor"
      :global t
      :lighter "Anki"
      :keymap (let ((map (make-sparse-keymap)))
                (define-key map (kbd "M-RET") 'my/anki-editor-insert-note)
                (define-key map (kbd "C-c C-c") 'anki-editor-push-notes)
                (define-key map (kbd "C-c C-r") 'anki-editor-retry-failure-notes)
                map
                ))

    (defun tag-anki-mode ()
      "快速插入填空"
      (interactive)
      (tag-word-or-region "{{c1:: " "}}"))

    (defun my/anki-editor-insert-note (&optional prefix)
      "Insert a note interactively.

Where the note subtree is placed depends on PREFIX, which is the
same as how it is used by `M-RET'(org-insert-heading)."
      (interactive "P")
      (message "Fetching note types...")
      (let* ((deck "java")
             (note-type "Cloze")
             (fields (progn
                       (message "Fetching note fields...")
                       (anki-editor--anki-connect-invoke-result "modelFieldNames" `((modelName . ,note-type)))))
             (note-heading ""))

        (anki-editor--insert-note-skeleton prefix
                                           deck
                                           (if (string-blank-p note-heading)
                                               ""
                                             note-heading)
                                           note-type
                                           fields)))

    )
  )

;;; packages.el ends here
