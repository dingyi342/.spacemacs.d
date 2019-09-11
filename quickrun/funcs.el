;;; packages.el - quickrun layer file for Spacemacs
;;
;; Copyright (c) 2018 Seong Yong-ju
;;
;; Author: Seong Yong-ju <sei40kr@gmail.com>
;; URL: https://github.com/sei40kr/spacemacs-quickrun
;;
;; This file is not part of GNU Emacs
;;
;;; License: MIT

;; quickrun

(defun spacemacs//quickrun-may-ask-args ()
  (if (not quickrun-option-args)
      (setq quickrun-option-args
            (read-string "quickrun arguments: "
                         (car quickrun--with-arg--history)
                         'quickrun--with-arg--history))))

(defun spacemacs//quickrun-may-ask-stdin-file ()
  (let* ((stdin-file-names
          (directory-files default-directory nil
                           (concat (quickrun--stdin-file-name) "*"))))
    (if (<= 2 (length stdin-file-names))
        (let* ((stdin-file-name
                (helm
                 :sources (helm-build-sync-source "quickrun stdin file"
                            :candidates stdin-file-names)
                 :input (concat quickrun--executed-file
                                (default-value 'quickrun-input-file-extension)))))
          (setq-local quickrun-input-file-extension
                      (concat "."
                              (file-name-extension stdin-file-name)))))))

(defun spacemacs/quickrun ()
  (interactive)
  (require 'quickrun)
  (spacemacs//quickrun-may-ask-args)
  (spacemacs//quickrun-may-ask-stdin-file)
  (if (eq evil-state 'visual)
    (quickrun-region (region-beginning) (region-end))
    (quickrun)))

(defun spacemacs/quickrun-with-arg ()
  (interactive)
  (setq quickrun-option-args nil)
  (spacemacs/quickrun))

(defun spacemacs/quickrun-create-stdin-file ()
  (interactive)
  (require 'tramp)
  (helm :sources (helm-make-source "Create File" 'helm-source-ffiles)
        :input (concat (buffer-file-name) ".qrinput")
        :prompt "Filename: "
        :resume 'noresume
        :buffer "*helm create stdin file*"))
