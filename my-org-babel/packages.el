;;; packages.el --- my-org-babel layer packages file for Spacemacs.
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
;; https://github.com/diadochos/org-babel-eval-in-repl/wiki
;; 不打开ielm窗口，只想执行code block像eval一样，其他并不想。
;;; Code:

(defconst my-org-babel-packages
  '(
    eval-in-repl
    org-babel-eval-in-repl
    )
  )

(defun my-org-babel/init-eval-in-repl ()
  (use-package eval-in-repl
    :config
    )
  )

(defun my-org-babel/init-org-babel-eval-in-repl ()
  (use-package org-babel-eval-in-repl
    :after ob
    :config
    ;; (with-eval-after-load "ob"
    ;;   (require 'org-babel-eval-in-repl)
    ;;   ;; (define-key org-mode-map (kbd "C-<return>") 'ober-eval-in-repl)
    ;;   ;; (define-key org-mode-map (kbd "M-<return>") 'ober-eval-block-in-repl)
    ;;   )

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "E" 'ober-eval-block-in-repl
      "M" 'ober-eval-block-in-repl
      )

    (with-eval-after-load "eval-in-repl"
      (setq eir-jump-after-eval nil))

    (defun org-ctrl-return-around (org-fun &rest args)
      "Run `ober-eval-in-repl' if in source code block and `org-insert-heading-respect-content' otherwise."
      (if (org-in-block-p '("src" "example"))
          (ober-eval-in-repl)
        (apply org-fun args)))
    (advice-add 'org-insert-heading-respect-content :around #'org-ctrl-return-around)

    (defun org-meta-return-around (org-fun &rest args)
      "Run `ober-eval-block-in-repl' if in source code block or example block and `org-meta-return' otherwise."
      (if (org-in-block-p '("src" "example"))
          (ober-eval-block-in-repl)
        (apply org-fun args))
      ;; 并不想用ielm
      ;; (kill-buffer "*ielm*")
      ;; (delete-other-windows)
      )
    (advice-add 'org-meta-return :around #'org-meta-return-around)

;;     (defun org-meta-return-around (org-fun &rest args)
;;       "Run `ober-eval-in-repl' if in source code block,
;; `ober-eval-block-in-repl' if at header,
;; and `org-meta-return' otherwise."
;;       (if (org-in-block-p '("src"))
;;           (let* ((point (point))
;;                  (element (org-element-at-point))
;;                  (area (org-src--contents-area element))
;;                  (beg (copy-marker (nth 0 area))))
;;             (if (< point beg)
;;                 (ober-eval-block-in-repl)
;;               (ober-eval-in-repl)))
;;         (apply org-fun args)))
;;     (advice-add 'org-meta-return :around #'org-meta-return-around)

    (add-hook #'org-ctrl-c-ctrl-c-hook #'org-babel-eval-exp)
    (defun org-babel-eval-exp ()
      (interactive)
      (let ((exp (nth 1 (org-babel-get-src-block-info))))
        (if (string-equal "emacs-lisp" (nth 0 (org-babel-get-src-block-info)))
            (eval (read exp)))
        (if (string-equal "shell" (nth 0 (org-babel-get-src-block-info)))
            (async-shell-command exp))
        )
      )

    )
  )
;;; packages.el ends here
