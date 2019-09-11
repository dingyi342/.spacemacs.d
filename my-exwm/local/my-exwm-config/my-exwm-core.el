;;; my-exwm-core.el --- my exwm core function.       -*- lexical-binding: t; -*-

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


;;; keybinding funcs.
(defun exwm-input-set-key* (key command)
  "This function is similar with `exwm-input-set-key', the
different is that `exwmx-input-set-key' protect `key' from
being override by other minor modes with the help of `bind-key*'."
  (exwm-input-set-key key command)
  (bind-key* key command))

(defun exwm-input-set-keys (key command &rest bindings)
  "exwm-input-set-key accept multiple key-comanad paris"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(defun exwm-input-set-keys* (key command &rest bindings)
  (while key
    (exwm-input-set-key* (kbd key)
                         ;; `(lambda ()
                         ;; (interactive)
                         ;; (start-process-shell-command ,command nil ,command))
                         command
                         )
    (setq key     (pop bindings)
          command (pop bindings))))


;;; copy/paste
(defun my/exwm-copy ()
  (interactive)
  (if (derived-mode-p 'exwm-mode)
      (exwm-input--fake-key (car (string-to-list (kbd "C-<insert>"))))
    (kill-ring-save)
    )
  )

(defun my/exwm-paste ()
  (interactive)
  (if (derived-mode-p 'exwm-mode)
      (exwm-input--fake-key (car (string-to-list (kbd "S-<insert>"))))
    (yank)
    ;; (general-simulate-key "S-<insert>")
    )
  )


;;; escape everything
;; (global-set-key (kbd "C-M-g") (lambda ()
;;                                 (interactive)
;;                                 (abort-recursive-edit)
;;                                 (keyboard-escape-quit)
;;                                 (keyboard-quit)
;;                                 ))
;; (defun doom/escape ()
;;   "Run `doom-escape-hook'."
;;   (interactive)
;;   (cond ((minibuffer-window-active-p (minibuffer-window))
;;          ;; quit the minibuffer if open.
;;          (abort-recursive-edit))
;;         ;; don't abort macros
;;         ((or defining-kbd-macro executing-kbd-macro) nil)
;;         ;; Back to the default
;;         ((keyboard-quit))))

;; (global-set-key [remap keyboard-quit] #'doom/escape)

(defun my/exwm-escape ()
  (interactive)
  (let ((inhibit-message t))
    (when (active-minibuffer-window)
      (select-window (active-minibuffer-window))
      (minibuffer-keyboard-quit))
    (ignore-errors
      (when (eq hydra-deactivate t)
        (setq hydra-deactivate t)
        (hydra-keyboard-quit))
      (when (helm--alive-p)
        (helm-keyboard-quit)))
    ;; layout
    (if (= (count-windows) 2)
        (let* ((window-tree (car (window-tree)))
               (current-split-vertical-p (car window-tree))
               (first-window (nth 2 window-tree))
               (second-window (nth 3 window-tree))
               (second-window-state (window-state-get second-window))
               (splitter #'split-window-horizontally))
          (delete-other-windows first-window)
          ;; `window-state-put' also re-selects the window if needed, so we don't
          ;; need to call `select-window'
          (window-state-put second-window-state (funcall splitter)))
      ;; (error "Can't toggle window layout when the number of windows isn't two.")
      )
    ;; quit recursive
    (ignore-errors
      (abort-recursive-edit) ;C-]
      (keyboard-escape-quit) ;M-ESC ESC
      (keyboard-quit)        ;C-g
      )
    ;; (sleep-for 3)
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (force-window-update))))

(exwm-input-set-key (kbd "s-g") #'my/exwm-escape)

(provide 'my-exwm-core)
;;; my-exwm-core.el ends here

