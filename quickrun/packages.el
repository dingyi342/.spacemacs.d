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

(setq quickrun-packages
      '(golden-ratio
        popwin
        quickrun))

(defun quickrun/post-init-golden-ratio ()
  (eval-after-load 'golden-ratio
    '(push "*quickrun*" golden-ratio-exclude-buffer-names)))

(defun quickrun/pre-init-popwin ()
  (eval-after-load 'popwin
    '(push
      '("*quickrun*"
        :dedicated t
        :position bottom
        :stick t
        :noselect nil
        :height 0.4)
      popwin:special-display-config)))

(defun quickrun/init-quickrun ()
  (use-package quickrun
    :defer (spacemacs/defer)
    :init
    (progn
      (setq
       quickrun-focus-p nil
       quickrun-option-shebang t)
      (spacemacs/declare-prefix "cq" "quickrun")
      (spacemacs/set-leader-keys
        "cqq" #'spacemacs/quickrun
        "cqa" #'spacemacs/quickrun-with-arg
        "cqh" #'helm-quickrun
        "cqs" #'spacemacs/quickrun-create-stdin-file)
      (eval-after-load 'evil
        '(evil-define-key 'normal quickrun--mode-map "q" #'quit-window)))))
