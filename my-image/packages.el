;;; packages.el --- image layer packages file for Spacemacs.
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

(defconst my-image-packages
  '(
    org-attach-screenshot
    (screenshot :location local) ; https://www.emacswiki.org/emacs/download/screenshot.el
    (my-org-screenshot :location local)
    (my-deepin-screenshot :location local)
    gif-screencast
    camcorder
    )
  )

(defun my-image/init-org-attach-screenshot ()
  (use-package org-attach-screenshot
    ))

(defun my-image/init-screenshot ()
  (use-package screenshot
    ))


(defun my-image/init-my-org-screenshot ()
  (use-package my-org-screenshot
    :config
    ;; (comma-def
    ;;   "S" 'my-org-screenshot)
    ;; (global-set-key (kbd "C-c s c") 'my-org-screenshot)
    (global-set-key (kbd "C-c s c") 'my-org-screenshot-autohide)
    )
  )

(defun my-image/init-my-deepin-screenshot ()
  (use-package my-deepin-screenshot
    :commands (my-deepin-screenshot
               my-org-deepin-screenshot)
    :config
    (setq my-deepin-screenshot-dir "~/OneDrive/org/org-imgs/")
    ;; 我并不需要延时
    (setq my-deepin-screenshot-timer 0)
    ;; (comma-def
    ;;   "S" 'my-deepin-screenshot)
    ))

(defun my-image/init-gif-screencast ()
  (use-package gif-screencast
    :config
    (setq
     gif-screencast-program "scrot"
     gif-screencast-convert-program "convert"
     gif-screencast-optimize-program "Gifsicle"
     ;; gif-screencast-screenshot-directory
     )
    (with-eval-after-load 'gif-screencast
      (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
      (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))
    ))

(defun my-image/init-keycast ()
  (use-package keycast
    :config
    (setq keycast-insert-after 'doom-modeline--evil-substitute)
    (keycast-mode)
    ))

(defun my-image/init-camcorder ()
  "录制gif"
  (use-package camcorder
    :ensure-system-package (recordmydesktop)
    :config
    (general-def "<f8>" 'camcorder-stop)

    )
  )

;;; packages.el ends here
