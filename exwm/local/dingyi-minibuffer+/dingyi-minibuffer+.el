;;; dingyi-minibuffer+.el --- mibibuffer improvement.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <dingyi@dingyi>
;; Keywords: abbrev

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

;; popup minibuffer

;;; Code:
(defvar endless/popup-frame-parameters
  '((name . "MINIBUFFER")
    (minibuffer . only)
    (height . 1)
    ;; Ajust this one to your preference.
    (top . 200))
  "Parameters for the minibuffer popup frame.")

(defvar endless/minibuffer-frame
  (let ((mf (make-frame endless/popup-frame-parameters)))
    (iconify-frame mf) mf)
  "Frame holding the extra minibuffer.")

(defvar endless/minibuffer-window
  (car (window-list endless/minibuffer-frame t))
  "")

(defmacro with-popup-minibuffer (&rest body)
  "Execute BODY using a popup minibuffer."
  (let ((frame-symbol (make-symbol "selected-frame")))
    `(let* ((,frame-symbol (selected-frame)))
       (unwind-protect 
           (progn 
             (make-frame-visible endless/minibuffer-frame)
             (when (fboundp 'point-screen-height)
               (set-frame-parameter
                endless/minibuffer-frame 
                'top (point-screen-height)))
             (select-frame-set-input-focus endless/minibuffer-frame 'norecord)
             ,@body)
         (select-frame-set-input-focus ,frame-symbol)))))

(defun point-screen-height ()
  (* (/ (face-attribute 'default :height) 10) 2
     (- (line-number-at-pos (point))
        (line-number-at-pos (window-start)))))

(defun use-popup-minibuffer (function)
  "Rebind FUNCTION so that it uses a popup minibuffer."
  (let* ((back-symb (intern (format "endless/backup-%s" function)))
         (func-symb (intern (format "endless/%s-with-popup-minibuffer"
                                    function)))
         (defs `(progn
                  (defvar ,back-symb (symbol-function ',function))
                  (defun ,func-symb (&rest rest)
                    ,(format "Call `%s' with a poupup minibuffer." function)
                    ,@(list (interactive-form function))
                    (with-popup-minibuffer 
                     (apply ,back-symb rest))))))
    (message "%s" defs)
    (when (and (boundp back-symb) (eval back-symb))
      (error "`%s' already defined! Can't override twice" back-symb))
    (eval defs)
    (setf (symbol-function function) func-symb)))


;;; Try at own risk.
;;(use-popup-minibuffer 'read-from-minibuffer)
;;; This will revert the effect.
;; (setf (symbol-function #'read-from-minibuffer) endless/backup-read-from-minibuffer)
;; (setq endless/backup-read-from-minibuffer nil)


;;; minibuffer at the top
(setq initial-frame-alist
      '((left . 1) (minibuffer . nil)
        ;; You'll need to adjust the following 3 numbers.
        (top . 75) ; In pixels
        (width . 127) ; In chars
        (height . 31)))

(setq minibuffer-frame-alist
      '((top . 1) (left . 1) (height . 2)
        ;; You'll need to adjust the following number.
        (width . 127)))

(provide 'dingyi-minibuffer+)
;;; dingyi-minibuffer+.el ends here

