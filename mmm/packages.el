;;; packages.el --- mmm-package layer packages file for Spacemacs.
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

(defconst mmm-packages
  '(
    mmm-mode
    polymode
    poly-org
    )
)

(defun mmm/post-init-mmm-mode ()
  "mmm mode allow multiple Major Modes to coexist in one buffer"
  (use-package mmm-mode
    :config

    )
  )

(defun mmm/init-polymode ()
  "A framework for multiple major modes inside a single Emacs buffer."
  (use-package polymode
    :mode ("\.py$" . poly-python-sql-mode)
    :config
    (setq polymode-prefix-key (kbd "C-c n"))
    (define-hostmode poly-python-hostmode :mode 'python-mode)

    (define-innermode poly-sql-expr-python-innermode
      :mode 'sql-mode
      :head-matcher (rx "r" (= 3 (char "\"'")) (* (any space)))
      :tail-matcher (rx (= 3 (char "\"'")))
      :head-mode 'host
      :tail-mode 'host)

    (defun poly-python-sql-eval-chunk (beg end msg)
      "Calls out to `sql-send-region' with the polymode chunk region"
      (sql-send-region beg end))

    (define-polymode poly-python-sql-mode
      :hostmode 'poly-python-hostmode
      :innermodes '(poly-sql-expr-python-innermode)
      (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
      (define-key poly-python-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk))

    ;; Bug? Fix polymode kill chunk so it works.
    (defun polymode-kill-chunk ()
      "Kill current chunk."
      (interactive)
      (pcase (pm-innermost-span)
        (`(,(or `nil `host) ,beg ,end ,_) (delete-region beg end))
        (`(body ,beg ,_ ,_)
         (goto-char beg)
         (pm--kill-span '(body))
         ;; (pm--kill-span '(head tail))
         ;; (pm--kill-span '(head tail))
         )
        (`(tail ,beg ,end ,_)
         (if (eq beg (point-min))
             (delete-region beg end)
           (goto-char (1- beg))
           (polymode-kill-chunk)))
        (`(head ,_ ,end ,_)
         (goto-char end)
         (polymode-kill-chunk))
        (_ (error "Canoot find chunk to kill")))))
  )

(defun mmm/init-poly-org ()
  (use-package poly-org
    :config
    )
  )

;;; packages.el ends here
