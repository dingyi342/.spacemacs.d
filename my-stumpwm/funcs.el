;;; funcs.el --- funcs.el                            -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun stumpwm/copy-to-today-file ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      ;; (find-file-other-window my-today-file-path)
      (find-file my-today-file-path)
      ;; (deft-new-file-named (format-time-string "%Y-%m-%d"))
      (goto-char (point-max))
      (ignore-errors
        (org-meta-return))
      ;; (let ((current-prefix-arg '(16)))
      ;;   (call-interactively 'org-time-stamp))
      (insert (shell-command-to-string "echo -n $(date +%T)"))
      (newline)
      ;;(evil-insert-state)
      (evil-paste-after 1)
      ;; (evil-insert-state)
      (save-buffer)
      ;; (quit-window)
      )
    )
  )

(defun stumpwm/input ()
  (interactive)
  (let (
        (minibuffer-auto-raise t)
        (resize-mini-windows 'grow-only)
        )
    (if (region-active-p)
        (progn
          (kill-ring-save (region-beginning) (region-end))
          ;; send Return
          ;; (general-simulate-key "Ret")
          )
      (kill-new (read-from-minibuffer "input: "))
      )
    )
  )

(defun stumpwm/thing-at-point ()
  (interactive)
  (if (region-active-p)
      (progn
        (kill-ring-save (region-beginning) (region-end))
        )
    (kill-new (thing-at-point 'word 'no-properties)))
  )

;; (defun stumpwm/input-buffer ()
;;   (interactive)
;;   )

(defun stumpwm/weibo ()
  (browse-url "https://www.weibo.com/dingyi342/home?wvr=5")
  )

(provide 'funcs)
;;; funcs.el ends here
