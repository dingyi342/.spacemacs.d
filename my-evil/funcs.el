;;; funcs.el --- my evil elisp code.                 -*- lexical-binding: t; -*-

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

;(defun evil-stop-execute-in-insert-state ()
;  (when (and (not (eq this-command #'evil-execute-in-insert-state))
;             (not (minibufferp)))
;    (remove-hook 'post-command-hook 'evil-stop-execute-in-insert-state)
;    (when (buffer-live-p evil-execute-in-insert-state-buffer)
;      (with-current-buffer evil-execute-in-insert-state-buffer
;        (if (and (eq evil-previous-state 'visual)
;                 (not (use-region-p)))
;            (progn
;              (evil-change-to-previous-state)
;              (evil-exit-visual-state))
;          (evil-change-to-previous-state))))
;    (setq evil-execute-in-insert-state-buffer nil)))
;
;(evil-define-command evil-execute-in-insert-state ()
;  "Execute the next command in insert state."
;  (add-hook 'post-command-hook #'evil-stop-execute-in-insert-state t)
;  (setq evil-execute-in-insert-state-buffer (current-buffer))
;  (cond
;   ((evil-visual-state-p)
;    (let ((mrk (mark))
;          (pnt (point)))
;      (evil-insert-state)
;      (set-mark mrk)
;      (goto-char pnt)))
;   (t
;    (evil-insert-state)))
;  (evil-echo "Switched to Emacs state for the next command ..."))

(provide 'funcs)
;;; funcs.el ends here
