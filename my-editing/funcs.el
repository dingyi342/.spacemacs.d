;;; funcs.el --- my-editing funcs.                   -*- lexical-binding: t; -*-

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
;;; ins-brackets
(defun ins-brackets ()
  (interactive)
  (cond ((eq major-mode 'term-mode)
         (term-send-raw-string "[]")
         (term-send-raw-string "^B"))
        ((region-active-p)
         (lispy--surround-region "[" "]"))
        (t
         (insert "[]")
         (backward-char)
         (set-enim)
         (evil-insert 1))))
;; (global-set-key "ρ" 'ins-brackets)

;;; exwm-ins-brackets
(defun exwm-ins-brackets ()
  (interactive)
  (cond ((eq major-mode 'exwm-mode)
         (exwmx-sendstring--send "[]")
         (run-at-time "0.1 sec" nil (lambda () (exwm-input--fake-key ?\C-b)))
         )))
;;; ins-parens
(defun ins-parens ()
  (interactive)
  (cond ((eq major-mode 'term-mode)
         (term-send-raw-string "()")
         (term-send-raw-string "^B"))
        ((region-active-p)
         (lispy--surround-region "(" ")"))
        (t
         (insert "()")
         (backward-char)
         (set-enim)
         (evil-insert 1))))
;; (global-set-key "φ" 'ins-parens)

;;; exwm-ins-parens
(defun exwm-ins-parens ()
  (interactive)
  (cond ((eq major-mode 'exwm-mode)
         (exwmx-sendstring--send "()")
         (run-at-time "0.1 sec" nil (lambda () (exwm-input--fake-key ?\C-b)))
         )))

;;; ins-braces
(defun ins-braces ()
  (interactive)
  (cond ((eq major-mode 'term-mode)
         (term-send-raw-string "{}")
         (term-send-raw-string "^B"))
        ((region-active-p)
         (lispy--surround-region "{" "}"))
        (t
         (insert "{}")
         (backward-char)
         (set-enim)
         (evil-insert 1))))
;; (global-set-key "σ" 'ins-braces)

;;; exwm-ins-braces
(defun exwm-ins-braces ()
  (interactive)
  (cond ((eq major-mode 'exwm-mode)
         (exwmx-sendstring--send "{}")
         (run-at-time "0.1 sec" nil (lambda () (exwm-input--fake-key ?\C-b)))
         )))

;;; ins-quotes

(defun ins-quotes ()
  (interactive)
  (cond ((eq major-mode 'term-mode)
         (term-send-raw-string "\"\"")
         (term-send-raw-string "^B"))
        ((region-active-p)
         (lispy--surround-region "\"" "\""))
        (t
         (insert "\"\"")
         (backward-char)
         (set-enim)
         (evil-insert 1))))
;; (global-set-key "θ" 'ins-quotes)

;;; exwm-ins-quotes
(defun exwm-ins-quotes ()
  (interactive)
  (cond ((eq major-mode 'exwm-mode)
         (exwmx-sendstring--send "\"\"")
         (run-at-time "0.1 sec" nil (lambda () (exwm-input--fake-key ?\C-b)))
         )))

;; (global-set-key "χ" 'lispy-right)
;; (global-set-key "η" 'lispy-left)

(provide 'funcs)
;;; funcs.el ends here
