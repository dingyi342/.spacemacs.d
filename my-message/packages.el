;;; packages.el --- my-message layer packages file for Spacemacs.
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

(defconst my-message-packages
  '(
    )
)

;;; 不显示一些消息
;; @https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer, quit signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer
                                beginning-of-line
                                end-of-line
                                quit
                                windmove-do-window-select
                                )))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;;; packages.el ends here
