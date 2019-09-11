;;; packages.el --- exwm layer packages file for Spacemacs.
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
;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `exwm-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `exwm/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `exwm/pre-init-PACKAGE' and/or
;;   `exwm/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
;;; packages
(defconst exwm-packages
  '(
    cl-generic
    desktop-environment
    helm-exwm
    (evil-exwm-state :location (recipe :fetcher github
                                       :repo "dingyi342/evil-exwm-state"))
    (xelb :location (recipe :fetcher github
                            :repo "ch11ng/xelb")
          :step pre)
    (exwm :location (recipe :fetcher github
                            :repo "ch11ng/exwm")
          :step pre)
    exwm-edit
    (symon-lingr :excluded t)
    exwm-x
    (dingyi-minibuffer+ :location local)
    ;; minibuffer-line
    ;; helm
    )
  )

;;; cl-generic
(defun exwm/init-cl-generic ()
  (use-package cl-generic
    :demand))

;;; evil-exwm-state
(defun exwm/init-evil-exwm-state ()
  (use-package evil-exwm-state
    :init
    (spacemacs/define-evil-state-face "exwm" "firebrick1")
    (spacemacs/define-evil-state-face "exwm-insert" "chartreuse3")
    :config
    ;; (define-key evil-exwm-state-map "i" 'evil-exwm-insert-state)
    (general-def evil-exwm-state-map
      "i" 'evil-exwm-insert-state
      "a" 'evil-escape
      "s" 'evil-escape
      )

;;;; evil-escape-func
    (advice-add 'evil-escape-func :override #'dingyi/evil-escape-func)
     (defun dingyi/evil-escape-func ()
       "Return the function to escape from everything."
       (pcase evil-state
         (`normal (evil-escape--escape-normal-state))
         (`motion (evil-escape--escape-motion-state))
         (`insert 'evil-normal-state)
         (`emacs (evil-escape--escape-emacs-state))
         (`hybrid (evil-escape--escape-emacs-state))
         (`evilified (evil-escape--escape-emacs-state))
         (`visual 'evil-exit-visual-state)
         (`replace 'evil-normal-state)
         (`lisp 'evil-lisp-state/quit)
         (`iedit 'evil-iedit-state/quit-iedit-mode)
         (`iedit-insert 'evil-iedit-state/quit-iedit-mode)
         (`multiedit 'evil-multiedit-abort)
         (`multiedit-insert 'evil-multiedit-state)
         (`exwm-insert 'evil-exwm-state)
         (`exwm 'evil-exwm-state)
         (_ (evil-escape--escape-normal-state))))

     ;; ;; ensure that when char mode is left, state is restored to normal
     ;; (advice-add 'exwm-input-grab-keyboard :after (lambda (&optional id)
     ;;                                                (evil-exwm-state)))
     ;; ;; ensure that when char mode is entered, input state is activated
     ;; (advice-add 'exwm-input-release-keyboard :after (lambda(&optional id)
     ;;                                                   (evil-exwm-insert-state)))

;;;; end

    ))


;;; exwm
(defun exwm/init-exwm ()
  (use-package exwm
    :init
    (comma-def exwm-mode-map
     "t" (lambda ()
              (interactive)
              (exwm-input--fake-key ?\C-t))
     "c" (lambda ()
           (interactive)
             (exwm-input--fake-key ?\C-c))
     )

    :config
    (define-key exwm-mode-map (kbd "C-c") nil)
    (remove ?\C-c exwm-input-prefix-keys)

    (setq
     exwm-workspace-number 10          ;初始workspace number
     ;; exwm-workspace-index-map
     exwm-workspace-current-index 1
     exwm-workspace-show-all-buffers nil ;不显示其他工作区的buffers，只显示当前工作区的buffers
     exwm-layout-show-all-buffers nil
     )

    ;; (add-hook 'after-init-hook #'exwm-layout-hide-mode-line)
    ;; (add-hook 'exwm-init-hook #'exwm-layout-hide-mode-line)
    ;; (add-hook 'exwm-manage-finish-hook #'exwm-layout-hide-mode-line)

;;;; hide echo area
    ;;(setq exwm-workspace-minibuffer-position 'bottom)
    ;;(setq exwm-workspace-display-echo-area-timeout 0.2)


;;;; 很多程序会用到 escape来退出一些输入状态的。
;; 本来想用来evil-escape 在exwm-mode里的，发现evil并没有多大用。
;; (exwm-input-set-key [escape] 'evil-escape)

;; (define-key key-translation-map [?\C-\[] [(control left_bracket)])
;; (define-key key-translation-map [escape] [?\e])
;; (define-key function-key-map [escape] nil)
;; (define-key function-key-map [?\e] nil)
;; (when (boundp 'local-function-key-map)
;;   ;;(define-key local-function-key-map [escape] nil)
;;   (defun remove-escape-from-local-function-key-map ()
;;     (define-key local-function-key-map [?\e] nil)
;;     (define-key local-function-key-map [escape] nil))
;;   (add-hook 'term-setup-hook 'remove-escape-from-local-function-key-map))
;; ;;(exwm-input-set-key [?\C-\] 'evil-escape)
;; (global-set-key (kbd "C-g") 'evil-escape)
;; (global-set-key (kbd "C-\[") 'evil-escape)

;;(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))

;;;; passthrough nil
(setq exwm-input-line-mode-passthrough nil)

;;;; 其他设置
     (setq use-dialog-box nil)


;;    (setq exwm-workspace-minibuffer-position 'top)
;;    (setq exwm-workspace--display-echo-area-timer 9999)
    ;; (exwm-input-set-key (kbd "<escape>")
                        ;; (lambda () (interactive)
                          ;; (evil-or-exwm-escape)))

    ;; (evil-set-initial-state 'exwm-mode 'insert)
    ;; t,设置line mode拦截所有按键事件到emacs
    ;; (setq exwm-input-line-mode-passthrough t)


    ;; 进入insert state自动切换到char mode
    ;; (add-hook 'evil-insert-state-entry-hook 'exwm-input-char-mode)
    ;;(exwm-input-set-key (kbd "<escape>")
    ;;                    (lambda () (interactive)
    ;;                      (evil-force-normal-state)
    ;; 执行两次，有时候用 esc取消key sequence时，会导致在exwm mode中，虽然显示的是line-mode，但是仍然可以输入。
    ;;                       ;; (evil-force-normal-state)
    ;;                      ))

    ;; C-[ 无法绑定，估计就何tab 与 c-i的关系，需要先解绑这两个关系。
    ;;(exwm-input-set-key (kbd "C-\[")
    ;;                   (lambda ()
    ;;                     (interactive)
    ;;                     (evil-force-normal-state)
    ;;                     (exwm-input-line-mode)
    ;;                     ))

    ;;(exwm-input-set-key [?\C-\[]
    ;                  (lambda ()
    ;                    (interactive)
    ;                    (evil-force-normal-state)
    ;                    (exwm-input-line-mode)
    ;                    ))

    ;; (exwm-input-set-key (kbd "<escape>")
                        ;; (lambda ()
                          ;; (interactive)
                          ;; (evil-force-normal-state)
                          ;; (exwm-input-line-mode)
                          ;; ))

    ;; (exwm-input-line-mode)
    ;; (exwm-input-char-mode)
    ;; 进入 line mode自动切换到 normal state
    ;; (exwm-input-set-key (kbd "s-a") 'evil-insert-state)
    ;; (exwm-input-set-key (kbd "s-n") 'evil-force-normal-state)

    ;; (add-hook 'evil-normal-state-entry-hook 'exwm-input-line-mode)

    ;; (add-hook 'evil-normal-state-entry-hook
    ;;           (lambda ()
    ;;             (interactive)
    ;;             (with-current-buffer (window-buffer)
    ;;               (when (eq major-mode 'exwm-mode)
    ;; ;;                 (setq exwm-input-line-mode-passthrough t)
    ;;                 (exwm-input-line-mode)
    ;;                 ))))
    ;; (add-hook 'evil-normal-state-exit-hook
    ;;           (lambda ()
    ;;             (interactive)
    ;;             (with-current-buffer (window-buffer)
    ;;               (when (eq major-mode 'exwm-mode)
    ;; ;;                 (setq exwm-input-line-mode-passthrough nil) ;;                 (exwm-input-char-mode) ;;                 ;; (exwm-layout-hide-mode-line) ;;                 )))) (display-time-mode 1)
    (setq display-time-string-forms '((format-time-string "%H:%M" now)))

    ;; (define-key exwm-mode-map (kbd "SPC") 'full-prefix)

    ;; (define-key exwm-mode-map (kbd "SPC") 'spacemacs-default-map)
    ;; (define-key exwm-mode-map (kbd "SPC") (lookup-key spacemacs-default-map (kbd "SPC")))

;;;; exwm-input-prefix-keys char-mode下通过的prefix-keys
    ;; (push ?\  exwm-input-prefix-keys) ;; ?\ 是32,就直接打个空格就表示空格了。
    (push ?\M-m exwm-input-prefix-keys)
    ;; (push ?\M-o exwm-input-prefix-keys)
    ;; (push ?\M-: exwm-input-prefix-keys)
    ;; 这个是要传递一串纯数字作为参数。
    (push (car (string-to-list (kbd "s-SPC"))) exwm-input-prefix-keys)
    (push (car (string-to-list (kbd "SPC"))) exwm-input-prefix-keys)
    (push ?\, exwm-input-prefix-keys)
    (push ?\; exwm-input-prefix-keys)
    ;; (push ?\C-w exwm-input-prefix-keys)
    (push ?\: exwm-input-prefix-keys)
    ;; (push ?\s-SPC exwm-input-prefix-keys)
    ;; (push (car (string-to-list (kbd "M-SPC"))) exwm-input-prefix-keys)
    ;; (push (car (string-to-list (kbd "s-SPC"))) exwm-input-prefix-keys)
    (push ?\s-\s exwm-input-prefix-keys)
    ;; (push ?\M-\s exwm-input-prefix-keys)
    ;; (push ?\m exwm-input-prefix-keys) ;添加m
    ;; (delete ?\m exwm-input-prefix-keys) ;删除m
    (delete ?\s exwm-input-prefix-keys) ;删除空格
    (delete ?\C-c exwm-input-prefix-keys)
    (delete (car (string-to-list (kbd "C-c Return"))) exwm-input-prefix-keys)

    (define-key exwm-mode-map (kbd "s-<return>") 'exwm-workspace-move-window)
    (exwm-input-set-key (kbd "s-<escape>") 'exwm-reset)
    ;;让,获得SPC o的keymap,并在line-mode上能用，因为已经不用,继承space-default-map了，
    ;; (define-key exwm-mode-map (kbd ",")
      ;; (lookup-key spacemacs-default-map
                  ;; (kbd "o"))
      ;; )
    ;; (define-key exwm-mode-map (kbd ", SPC") 'spacemacs-default-map)

    ;; (define-key exwm-mode-map (kbd "σ")
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (exwm-input--fake-key ?\C-c)))

    ;; (define-key exwm-mode-map (kbd "φ")
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (exwm-input--fake-key ?\C-c)))
;;;; evil exwm vim化应用
    ;; exwm集成evil并没什么用，应该可以vim化应用。
    ;; ensure that when char mode is left, state is restored to normal
    (advice-add 'exwm-input-grab-keyboard :after (lambda (&optional id)
                                                   ;; (evil-normal-state)
                                                   (evil-exwm-state)
                                                   ))
    ;; ensure that when char mode is entered, input state is activated
    (advice-add 'exwm-input-release-keyboard :after (lambda(&optional id)
                                                      ;; (evil-insert-state)
                                                      (evil-exwm-insert-state)
                                                      ))


;;;; s-up/down调节屏幕亮度
;;;; s-right/left调节声音大小
;;;; C-q send next key
;;;; prefix

    ;; in normal state/line mode, use the familiar i key to switch to input state
    ;; (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-release-keyboard)
    ;; (push ?\i exwm-input-prefix-keys)

    ;; regular space leader keys in line mode
    ;; (defun spacemacs//exwm-convert-key-to-event (key)
    ;;   "Converts something from (kbd ...) format to something suitable for
    ;; exwm-input-prefix-keys"
    ;;   (let ((key (kbd key)))
    ;;     (if (and (sequencep key)
    ;;              (= (length key) 1))
    ;;         (etypecase key
    ;;           (string (string-to-char key))
    ;;           (vector (elt key 0)))
    ;;       (error "cannot convert to key event: %s" key))))

    ;; ;; (push ?\  exwm-input-prefix-keys)
    ;; (push (spacemacs//exwm-convert-key-to-event dotspacemacs-leader-key) exwm-input-prefix-keys)
    ;; (push (spacemacs//exwm-convert-key-to-event dotspacemacs-emacs-leader-key) exwm-input-prefix-keys)
    ;; ;; introduce new universal leader: s-SPC
    ;; buggy:
    ;; (exwm-input-set-key (kbd "s-SPC") spacemacs-default-map)

    ;; Universal Get-me-outta-here
    (push ?\C-g exwm-input-prefix-keys)
    ;; Universal Arguments
    (push ?\C-u exwm-input-prefix-keys)
    (push ?\C-0 exwm-input-prefix-keys)
    (push ?\C-1 exwm-input-prefix-keys)
    (push ?\C-2 exwm-input-prefix-keys)
    (push ?\C-3 exwm-input-prefix-keys)
    (push ?\C-4 exwm-input-prefix-keys)
    (push ?\C-5 exwm-input-prefix-keys)
    (push ?\C-6 exwm-input-prefix-keys)
    (push ?\C-7 exwm-input-prefix-keys)
    (push ?\C-8 exwm-input-prefix-keys)
    (push ?\C-9 exwm-input-prefix-keys)
    ;; C-c, C-x are needed for copying and pasting
    (delete ?\C-x exwm-input-prefix-keys)
    (delete ?\C-c exwm-input-prefix-keys)
    ;; We can use `M-m h' to access help
    (delete ?\C-h exwm-input-prefix-keys)
    ;; introduce leader for running programs
    (spacemacs/declare-prefix "&" "exwm-run")
    (spacemacs/set-leader-keys "&s" 'spacemacs/exwm-launch-split-below)
    (spacemacs/set-leader-keys "&v" 'spacemacs/exwm-launch-split-right)

;;;; 希腊字母 ；
    (general-def exwm-mode-map
;; q-θ r-ρ y-υ u-ψ i-ϊ p-π f-φ h-η k-κ x-χ c-σ b-β n-ν m-μ
      "ρ" 'exwm-ins-brackets
      "φ" 'exwm-ins-parens
      "ϊ" 'exwm-ins-braces
      "θ" 'exwm-ins-quotes
      "χ" 'lispy-right
      "η" 'lispy-left
      "σ" 'exwm-C-c
      )


;;;; M-数字 切换窗口
    (define-key exwm-mode-map (kbd "M-0") 'winum-select-window-0)
    (define-key exwm-mode-map (kbd "M-1") 'winum-select-window-1)
    (define-key exwm-mode-map (kbd "M-2") 'winum-select-window-2)
    (define-key exwm-mode-map (kbd "M-3") 'winum-select-window-3)
    (define-key exwm-mode-map (kbd "M-4") 'winum-select-window-4)
    (define-key exwm-mode-map (kbd "M-5") 'winum-select-window-5)
    (define-key exwm-mode-map (kbd "M-6") 'winum-select-window-6)

    ;; (define-key exwm-mode-map (kbd "C-t") nil)
    ;; (define-key exwm-mode-map (kbd "C-v") nil)
    ;; (define-key exwm-mode-map (kbd "C-c") nil)

    ;; ; 这好像不管用。
    ;; (exwm-input-set-key (kbd "s-0") 'winum-select-window-0)
    ;; (exwm-input-set-key (kbd "s-1") 'winum-select-window-1)
    ;; (exwm-input-set-key (kbd "s-2") 'winum-select-window-2)
    ;; (exwm-input-set-key (kbd "s-3") 'winum-select-window-3)
    ;; (exwm-input-set-key (kbd "s-4") 'winum-select-window-4)
    ;; (exwm-input-set-key (kbd "s-5") 'winum-select-window-5)

    ;; (exwm-input-set-key (kbd "M-:") )


    
;;;; 其他

    (defvar exwm--terminal-command "termite"
      "Terminal command to run.")

    (defvar exwm--locking-command "i3lock-fancy"
      "Command to run when locking session")

    (defvar exwm-app-launcher--prompt "$ "
      "Prompt for the EXWM application launcher")

    (defvar exwm--rofi-command "rofi -modi \"run,ssh\" -show run -font \"Input Mono Compressed 10\""
      "Command to start rofi launcher")

    (defvar exwm--hide-tiling-modeline t
      "Whether to hide modeline.")
;;;; smooth gesutre窗口自动移动到workspace 3
;; (require 'subr-x)

;; (add-hook 'exwm-manage-finish-hook
;;           ())
;;;; 显示时间
    (display-time-mode 1)
    (setq display-time-string-forms '((format-time-string "%H:%M" now)))

    ;; + Application launcher ('M-&' also works if the output buffer does not
    ;;   bother you). Note that there is no need for processes to be created by
    ;;   Emacs.
    (defun spacemacs/exwm-application-launcher (command)
      "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
      (interactive (list (read-shell-command exwm-app-launcher--prompt)))
      (start-process-shell-command command nil command))

    ;; (exwm-input-set-key (kbd "s-SPC") #'spacemacs/exwm-application-launcher)
    (exwm-input-set-key (kbd "s-p") #'spacemacs/exwm-application-launcher)

    ;; lock screen
    (exwm-input-set-key (kbd "<s-escape>")
                        (lambda () (interactive) (start-process "" nil exwm--locking-command)))

    ;; Workspace helpers

    (defvar exwm-workspace-switch-wrap t
      "Whether `spacemacs/exwm-workspace-next' and `spacemacs/exwm-workspace-prev' should wrap.")

    (defun spacemacs/exwm-workspace-next ()
      "Switch to next exwm-workspaceective (to the right)."
      (interactive)
      (let* ((only-workspace? (equal exwm-workspace-number 1))
             (overflow? (= exwm-workspace-current-index
                           (1- exwm-workspace-number))))
        (cond
         (only-workspace? nil)
         (overflow?
          (when exwm-workspace-switch-wrap
            (exwm-workspace-switch 0)))
         (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))

    (defun spacemacs/exwm-workspace-prev ()
      "Switch to next exwm-workspaceective (to the right)."
      (interactive)
      (let* ((only-workspace? (equal exwm-workspace-number 1))
             (overflow? (= exwm-workspace-current-index 0)))
        (cond
         (only-workspace? nil)
         (overflow?
          (when exwm-workspace-switch-wrap
            (exwm-workspace-switch (1- exwm-workspace-number))))
         (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

    ;; Quick swtiching between workspaces
    (defvar exwm-toggle-workspace 0
      "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")
    (defun exwm-jump-to-last-exwm ()
      (interactive)
      (exwm-workspace-switch exwm-toggle-workspace))
    (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
      (setq exwm-toggle-workspace exwm-workspace-current-index))

    ;; Rename buffer to window title
    ;; (defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
    ;; (add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

    ;; no mode line for floating windows
    ;; (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
    ;; (add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

    ;; per app settings

    ;; (defun exwm-start-in-char-mode ()
    ;;   (when (or (string= exwm-instance-name "emacs")
    ;;             (string= exwm-class-name "Termite")
    ;;             (string= exwm-class-name "URxvt")
    ;;             (string= exwm-class-name "XTerm")
    ;;             (string= exwm-class-name "libreoffice-startcenter"))
    ;;     (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

    ;; (add-hook 'exwm-manage-finish-hook 'exwm-start-in-char-mode)

    ;;make exwm windows default to char instead of line mode

    ;; (add-hook 'exwm-manage-finish-hook
              ;; (lambda () (call-interactively #'exwm-input-release-keyboard)
                ;; (exwm-layout-hide-mode-line)))

    ;send all keypresses to emacs in line mode
    ;; (setq exwm-input-line-mode-passthrough t)
    ;; (setq exwm-input-line-mode-passthrough nil)

    ;; (exwm-input-set-key (kbd "s-i")
                        ;; (lambda () (interactive)
                          ;; (exwm-input-toggle-mode)))


    ;; (exwm-input-set-key (kbd "s-o")
                        ;; (lambda ()
                          ;; (interactive)
                          ;; (exwm-input-toggle-mode)
                          ;; (org-capture)))

    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.
    ;; + We always need a way to go back to line-mode from char-mode
    ;; (exwm-input-set-key (kbd "C-c C-q") 'exwm-input-send-next-key)


;;;; exwm 窗口管理
    (exwm-input-set-key (kbd "s-r") 'exwm-reset)

    (exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
    (exwm-input-set-key (kbd "<s-tab>") #'exwm-jump-to-last-exwm)
    ;; + Bind a key to switch workspace interactively
    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

    (exwm-input-set-key (kbd "s-:") 'helm-M-x)
    (exwm-input-set-key (kbd "s-;") 'evil-ex)
    ;; Shell (not a real one for the moment)
    (exwm-input-set-key (kbd "C-'") #'spacemacs/default-pop-shell)
    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
    ;; Change buffers
    ;; (exwm-input-set-key (kbd "s-b") #'helm-mini)
    ;; (exwm-input-set-key (kbd "s-b") 'helm-exwm)

    (exwm-input-set-key (kbd "s-h") 'evil-window-left)
    (exwm-input-set-key (kbd "s-j") 'evil-window-down)
    (exwm-input-set-key (kbd "s-k") 'evil-window-up)
    (exwm-input-set-key (kbd "s-l") 'evil-window-right)
 ;; Moving Windows
    (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; Resize
    (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
    (exwm-input-set-key (kbd "M-s-j") #'spacemacs/shrink-window)
    (exwm-input-set-key (kbd "M-s-k") #'spacemacs/enlarge-window)
    (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)

    (exwm-input-set-key (kbd "s-!") (lambda () (interactive) (exwm-workspace-move-window 0)))
    (exwm-input-set-key (kbd "s-@") (lambda () (interactive) (exwm-workspace-move-window 1)))
    (exwm-input-set-key (kbd "s-#") (lambda () (interactive) (exwm-workspace-move-window 2)))
    (exwm-input-set-key (kbd "s-$") (lambda () (interactive) (exwm-workspace-move-window 3)))
    (exwm-input-set-key (kbd "s-%") (lambda () (interactive) (exwm-workspace-move-window 4)))
    (exwm-input-set-key (kbd "s-^") (lambda () (interactive) (exwm-workspace-move-window 5)))
    (exwm-input-set-key (kbd "s-&") (lambda () (interactive) (exwm-workspace-move-window 6)))
    (exwm-input-set-key (kbd "s-*") (lambda () (interactive) (exwm-workspace-move-window 7)))
    (exwm-input-set-key (kbd "s-(") (lambda () (interactive) (exwm-workspace-move-window 8)))
    (exwm-input-set-key (kbd "s-)") (lambda () (interactive) (exwm-workspace-move-window 9)))
    (push (elt (kbd "s-!") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-@") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-#") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-$") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-%") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-^") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-&") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-*") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-(") 0) exwm-input-prefix-keys)
    (push (elt (kbd "s-)") 0) exwm-input-prefix-keys)

    ;; (exwm-input-set-key (kbd "C-c C-c")
    ;;          (lambda ()
    ;;            (when (and exwm-class-name
    ;;                       (string=exwm-class-name "terminator"))
    ;;              ;; (exwm-input-set-next-key '(([?\C-c ?\C-q ?\C-c] . [?\C-c ?\C-c])))
    ;;              ;; (ewxm-input-set-local-simulation-keys '(([?\C-c ?\C-c] . ?\C-c)))
    ;;              ;; (exwm-input-set-next-key '([?\C-c ?\C-q ?\C-c]))
    ;;              ;; (exwm-input-send-next-key '([?\C-c]))
    ;;              ;; (exwm-input-send-next-key (kbd "C-c"))
    ;;              (call-interactively 'exwm-input-send-next-key)
    ;;              )))




    ; 希望出个混合式的，有mode-map就作为 mode 的组合键，没有就全局的，local自动覆盖全局的。
    ;; (evil-define-key nil exwm-mode-map (kbd ", q") 'exwm-input-send-next-key)
    ;; (spacemacs/set-cet-comma-leader-keyset-comma-leader-keysomma-leader-keys )
    ;; (spacemacs/set-leader-keys-for-major-mode exwm-mode-map
      ;; "q" 'exwm-input-send-next-key)

    ;; (read-key)
    ;; (read-event)

    ;; (define-key exwm-mode-map (kbd "M-m") 'full-prefix)
    ;; (define-key exwm-mode-map (kbd "M-m") 'full-prefix-emacs)

    ;; (define-key exwm-mode-map (kbd "SPC") 'full-prefix)
    ;; (define-key exwm-mode-map (kbd "M-m") 'full-prefix-emacs)
    ;; (define-key exwm-mode-map [?\i] 'exwm-input-char-mode)
    ;; (push ?\M-m exwm-input-prefix-keys)
    ;; (define-key exwm-mode-map (kbd "escape") 'exwm-reset)
    ;; (evil-define-key 'normal exwm-mode-map (kbd "i") 'exwm-input-char-mode)
    ;; (evil-define-key '(normal insert) exwm-mode-map (kbd "escape") 'evil-or-exwm-escape)

    ;;(add-hook 'exwm-manage-finish-hook
    ;;          (lambda ()
    ;;            (when (and exwm-class-name
    ;;                       (string=exwm-class-name "terminator"))
    ;;              (exwm-input-set-local-simulation-keys '(([?\C-c ?\C-q ?\C-c] . ?\C-c))))))

    ;; (add-hook 'exwm-manage-finish-hook
    ;;          (lambda ()
    ;;            (when (and exwm-class-name
    ;;                       (string=exwm-class-name "terminator"))
    ;;              (exwm-input-set-next-key '(([?\C-c ?\C-q ?\C-c] . [?\C-c ?\C-c]))))))



    ;; (add-hook 'exwm-manage-finish-hook
    ;;           (lambda ()
    ;;             (when (and exwm-class-name
    ;;                        (string=exwm-class-name "Terminator"))
    ;;               (exwm-reset)
    ;;               (exwm-input-char-mode))))

    ;; (advice-add 'select-window :after
    ;;             (lambda ()
    ;;               (interactive)
    ;;               (with-current-buffer (window-buffer)
    ;;                 (when (eq major-mode 'exwm-mode)
    ;;                   (exwm-reset)
    ;;                   (call-interactively 'exwm-input-char-mode)
                      ;; ))))

    
;;;; use-package exwm ends
    )
;;; exwm workspace
    (use-package exwm-workspace
      :config
      (setq
       exwm-workspace-number 10          ;初始workspace number
       ;; exwm-workspace-index-map 
       exwm-workspace-current-index 1
       exwm-workspace-show-all-buffers nil ;不显示其他工作区的buffers，只显示当前工作区的buffers
       exwm-layout-show-all-buffers nil
       )
;;;; s-数字 切换到具体的workspace
      ;; (exwm-input-set-key (kbd "s-f") #'spacemacs/exwm-layout-toggle-fullscreen)
      (exwm-input-set-key (kbd "<s-tab>") #'spacemacs/exwm-jump-to-last-exwm)
      (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

      ;; (exwm-input-set-keys* "s-1" (lambda () (interactive) (exwm-workspace-switch 0)))
      ;; (exwm-input-set-keys* "s-2" (lambda () (interactive) (exwm-workspace-switch 1)))
      ;; (exwm-input-set-keys* "s-3" (lambda () (interactive) (exwm-workspace-switch 2)))
      ;; (exwm-input-set-keys* "s-4" (lambda () (interactive) (exwm-workspace-switch 3)))
      ;; (exwm-input-set-keys* "s-5" (lambda () (interactive) (exwm-workspace-switch 4)))
      ;; (exwm-input-set-keys* "s-6" (lambda () (interactive) (exwm-workspace-switch 5)))
      ;; (exwm-input-set-keys* "s-7" (lambda () (interactive) (exwm-workspace-switch 6)))
      ;; (exwm-input-set-keys* "s-8" (lambda () (interactive) (exwm-workspace-switch 7)))
      ;; (exwm-input-set-keys* "s-9" (lambda () (interactive) (exwm-workspace-switch 8)))
      ;; (exwm-input-set-keys* "s-0" (lambda () (interactive) (exwm-workspace-switch 9)))

      ;; ;;     (push (elt (kbd "s-1") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-2") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-3") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-4") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-5") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-6") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-7") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-8") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-9") 0) exwm-input-prefix-keys)
      ;;     (push (elt (kbd "s-0") 0) exwm-input-prefix-keys)


      ;; exwm-workspace ends here.
      )
    
;;; exwm-config
  (use-package exwm-config
    :config
    (exwm-config-default))

;;; exwm-randr
  (use-package exwm-randr)

;;; exwm-systemtray
  (use-package exwm-systemtray
    :config
    (exwm-systemtray-enable)
    (setq exwm-systemtray-height 16)
    )
;;; exwm ends
  )
;;; xelb
(defun exwm/init-xelb ()
  (use-package xelb))

;;; desktop-environment
(defun exwm/init-desktop-environment ()
  (use-package desktop-environment
    :config
    ;;global minor modeb绑定按键到命令上，按键都是常规键盘上没有的那种键，可以绑定
    ;;包括调节亮度，声音，截图，锁屏等。这样就可以通过鼠标侧键来控制了。
    ;; 要安装一些命令行工具。
    ;; sudo pacman -S scrot slock
    ;;详见 https://github.com/DamienCassou/desktop-environment/tree/62fbceded526b8e35c90803bcf80e33ebfe8473a
    ;; 可以重新映射键，或者用我的有很多侧键的鼠标
;;;; 全局启用minor mode
    (desktop-environment-mode)
;;;; 禁用s-l用来锁屏
    ;; s-l默认绑定在切换到左侧窗口，看了源码，是定义到mode的map里的，优先级比较高。
    (define-key desktop-environment-mode-map (kbd "s-l") nil)
    (exwm-input-set-key (kbd "s-l") 'evil-window-right)
;;;; 调节声音，用 amixer
    (exwm-input-set-key (kbd "s-<right>") 'desktop-environment-volume-increment)
    (exwm-input-set-key (kbd "S-s-<right>") 'desktop-environment-volume-increment-slowly)
    (exwm-input-set-key (kbd "s-<left>") 'desktop-environment-volume-decrement)
    (exwm-input-set-key (kbd "S-s-<left>") 'desktop-environment-volume-decrement-slowly)
    (exwm-input-set-key (kbd "s-m") 'desktop-environment-toggle-mute)
    (exwm-input-set-key (kbd "s-M") 'desktop-environment-toggle-microphone-mute)
;;;; 调节亮度用 xrandr
    ;; brightnessctl用不起来，用xrandr
    ;; (exwm-input-set-key (kbd "s-<up>") 'desktop-environment-brightness-increment)
    ;; (exwm-input-set-key (kbd "S-s-<up>") 'desktop-environment-brightness-increment-slowly)
    ;; (exwm-input-set-key (kbd "s-<down>") 'desktop-environment-brightness-decrement)
    ;; (exwm-input-set-key (kbd "S-s-<down>") 'desktop-environment-brightness-decrement-slowly)
    (exwm-input-set-key (kbd "s-<up>") 'dingyi/brightness-adjust-increase-slowly)
    (exwm-input-set-key (kbd "S-s-<up>") 'dingyi/brightness-adjust-increase-fastly)
    (exwm-input-set-key (kbd "s-<down>") 'dingyi/brightness-adjust-decrease-slowly)
    (exwm-input-set-key (kbd "S-s-<down>") 'dingyi/brightness-adjust-decrease-fastly)
;;;; 截图，用scrot
    (exwm-input-set-key (kbd "s-S") 'desktop-environment-screenshot)
    (exwm-input-set-key (kbd "s-s") 'desktop-environment-screenshot-part)

;;;; 参考xmonad的快捷键
;; s-Enter s-space s-Q  s-t

;;;; ends
    ))

;;; exwm-edit
(defun exwm/init-exwm-edit ()
  "在chrome上时好时坏"
  (use-package exwm-edit
    :config
    ;; 这个钩子要在exwm-edit,让其主模式变成markdown
    (add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose)
    ;; (global-exwm-edit-mode 1)
    (exwm-input-set-key (kbd "s-e") 'exwm-edit--compose)
    (exwm-input-set-key (kbd "C-c '") nil)
    (exwm-input-set-key (kbd "C-c C-'") nil)
    (spacemacs/set-leader-keys-for-minor-mode 'exwm-edit-mode

      "m" 'dingyi/exwm-edit--finish
      "k" 'dingyi/exwm-edit--cancel
      "a" 'dingyi/exwm-edit--cancel
      )
    (define-key exwm-edit-mode-map (kbd "C-c '") 'dingyi/exwm-edit--finish)
    (define-key exwm-edit-mode-map (kbd "C-c C-'") 'dingyi/exwm-edit--finish)
    (define-key exwm-edit-mode-map (kbd "C-c C-c") 'dingyi/exwm-edit--finish)
    (define-key exwm-edit-mode-map (kbd "C-c C-k") 'dingyi/exwm-edit--cancel)

    ))

;;; symon-lingr
(defun exwm/init-symon-lingr ()
  (use-package symon-lingr
    :config
    ;; 影响minibuffer的显示，设置时间。
    (symon-mode)
    ))

;;; helm-exwm
(defun exwm/init-helm-exwm ()
  "helm-exwm显示所有exwm的buffer,过滤，kill,分组等"
  (use-package helm-exwm
    :after exwm
    :config
    ;; helm-mini 增加 Exwm，与其他区分开来，没什么用，太多了
    (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
    (setq helm-exwm-source (helm-exwm-build-source))
    (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                      helm-exwm-source
                                      helm-source-recentf))

    ;; (setq browse-url-generic-program "/usr/bin/chromium")
    ;; (setq terminal-generic-program "/usr/bin/terminator")
    ;; (setq music-generic-program "/usr/bin/chromium --profile-directory=Default --app-id=dkldghbjfphncdddjphinpedhldcemop")

    ;; 切换到浏览器buffer到当前窗口，helm-exwm..返回的就是buffer名而已。
    ;; (switch-to-buffer (helm-exwm-switch-browser))
    ;;隐藏所有exwm窗口，相当于显示桌面，win+d

    ;; create a multiple defuns
    (defconst my/exwm-app '("emacs" "firefox" "chromium" "dolphin" "anki" "guake" "xterm" "urxvt")
      "Alist of applications I tent to switch to frequently")
    (my/gen-exwm-app-funs)               ;my/exwm-swtich-$app


    ))

;;; exwm-x
(defun exwm/init-exwm-x ()
  (use-package exwm-x
    :config
    (setq exwmx-button-floating-button-line 'mode-line)
    ;; (setq exwmx-button-floating-button-line nil)

    (require 'exwm)
    (require 'exwm-x)
    ;; (require 'exwmx-xfce)
;;;; exwmx-example
    ;; (require 'exwmx-example)
    ;; (exwmx-input-set-key (kbd "C-t v") 'exwmx:file-browser)
    ;; (exwmx-input-set-key (kbd "C-t f") 'exwmx:web-browser)
    ;; (exwmx-input-set-key (kbd "C-t e") 'exwmx:emacs)
    ;; (exwmx-input-set-key (kbd "C-t c") 'exwmx-xfce-terminal)
    ;; (exwmx-input-set-key (kbd "C-t z") 'exwmx-floating-hide-all)
    ;; (exwmx-input-set-key (kbd "C-t C-c") 'exwmx-xfce-new-terminal)
    ;; (exwmx-input-set-key (kbd "C-t b") 'exwmx-switch-application)
    ;; (exwmx-input-set-key (kbd "C-t C-f") 'exwmx-floating-toggle-floating)

    ;; Disable dialog boxes since they are unusable in EXWM
    (setq use-dialog-box nil)

    ;; Set workspace number
    (setq exwm-workspace-number 10)

    ;; Set floating window border
    (setq exwm-floating-border-width 3)
    (setq exwm-floating-border-color "orange")

    ;; All buffers created in EXWM mode are named "*EXWM*".
    ;; You may want to change when a new window class name
    ;; or title is available. it in `exwm-update-class-hook'
    ;; and `exwm-update-title-hook', which are run
    ;; (add-hook 'exwm-update-class-hook #'exwmx-grocery--rename-exwm-buffer)
    ;; (add-hook 'exwm-update-title-hook #'exwmx-grocery--rename-exwm-buffer)

    ;; Manage `exwm-manage-finish-hook'
    ;; (add-hook 'exwm-manage-finish-hook #'exwmx-grocery--manage-finish-function)

    ;; Smart hide floating window
    (exwmx-floating-smart-hide)

    ;; Enable exwmx-button 只在浮动窗口上显示。
    ;; (exwmx-button-enable)


    (push ?\C-q exwm-input-prefix-keys)
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)
    ;; exwm-xim support
    (push ?\C-\\ exwm-input-prefix-keys)

    ;; (require 'switch-window)
    ;; ;; switch-window 'default input style do not work well with exwm.
    ;; (setq switch-window-input-style 'minibuffer)
    ;; (define-key exwm-mode-map (kbd "C-x o") #'switch-window)
    ;; (define-key exwm-mode-map (kbd "C-x 1") #'switch-window-then-maximize)
    ;; (define-key exwm-mode-map (kbd "C-x 2") #'switch-window-then-split-below)
    ;; (define-key exwm-mode-map (kbd "C-x 3") #'switch-window-then-split-right)
    ;; (define-key exwm-mode-map (kbd "C-x 0") #'switch-window-then-delete)

    ;; ;; The following example demonstrates how to use simulation keys to mimic the
    ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
    ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
    ;; sequence (of type vector or string), while DEST can also be a single key.
    (exwm-input-set-simulation-keys
     '(([?\C-b] . left)
       ([?\C-f] . right)
       ([?\C-p] . up)
       ([?\C-n] . down)
       ([?\C-a] . home)
       ([?\C-e] . end)
       ([?\M-v] . prior)
       ([?\C-v] . next)))

    (delete ?\C-b exwm-input-prefix-keys)
    (delete ?\C-f exwm-input-prefix-keys)
    (delete ?\C-p exwm-input-prefix-keys)
    (delete ?\C-n exwm-input-prefix-keys)
    (delete ?\C-a exwm-input-prefix-keys)
    (delete ?\C-e exwm-input-prefix-keys)
    (delete ?\C-h exwm-input-prefix-keys)
    (delete ?\C-v exwm-input-prefix-keys)
    (delete ?\M-v exwm-input-prefix-keys)
    (delete ?\M-h exwm-input-prefix-keys)

    ;; (advice-add 'exwmx-button-enable :override #'dingyi/exwmx-button-enable) 
    ;; Don't delete it
    (exwm-enable)

;;;; exwmx-appconfig
    (setq exwmx-appconfig-file "~/.emacs.d/exwm-x/exwmx-appconfig")
    ;; (exwm-input-set-key "s-a y" (lambda ()
    ;;                                 (interactive)
    ;;                                 (let ((chromium-youtube "/usr/bin/chromium --profile-directory=Default --app-id=phlfieknpahflbjejijbdlkdodpofpbk"))
    ;;                                   ;; (helm-exwm-switch "chromium-youtube" chromium-youtube)
    ;;                                 (exwmx-quickrun chromium-youtube t '(:alias :pretty-name))
    ;;                                 )))
    (setq switch-window-input-style 'minibuffer)
  ;; /usr/bin/chromium --profile-directory=Default --app-id=phlfieknpahflbjejijbdlkdodpofpbk


    (exwm-input-set-key (kbd "s-a m")
                        (lambda ()
                          (interactive)
                          ;; (switch-to-buffer (helm-exwm-switch-music))
                          (exwmx-quickrun "chromium-163music" t '(:pretty-name))
                          ))
    (exwm-input-set-key (kbd "s-a y")
                        (lambda ()
                          (interactive)
                          ;; (switch-to-buffer (helm-exwm-switch-music))
                          (exwmx-quickrun "chromium-youtube" t '(:pretty-name))
                          ))
    (exwm-input-set-key (kbd "s-a n")
                        (lambda ()
                          (interactive)
                          ;; (switch-to-buffer (helm-exwm-switch-music))
                          (exwmx-quickrun "chromium-onenote" t '(:pretty-name))
                          ))
    (exwm-input-set-key (kbd "s-a b")
                        (lambda ()
                          (interactive)
                          ;; (switch-to-buffer (helm-exwm-switch-music))
                          (exwmx-quickrun "chromium-bilibili" t '(:pretty-name))
                          ))



;;;; exmw-appconcifg-minor-mode
    (comma-def
      :definer 'minor-mode
      :keymaps 'exwmx-appconfig-mode
      "k" (general-simulate-key "C-c C-q")
      ;; "c" "C-c C-c"
      "c" (general-simulate-key "C-c C-c")
      )
    (spacemacs/set-leader-keys-for-minor-mode 'exwmx-appconfig-mode
      "k" 'exwmx-appconfig-ignore
      "c" 'exmwx-appconfig-finish
      )
;;;; 配置
   ;;(exwmx-input-set-key (kbd "<C-[>") 'evil-escape)
    (exwmx-input-set-key (kbd "<C-[>") [escape])

    (setq exwmx-sendstring-default-paste-key "S-<insert>")

    (exwmx-input-set-key (kbd "s-o") 'counsel-linux-app)
    (exwmx-input-set-key (kbd "s-d") 'exwmx-floating-hide-all)
    (exwmx-input-set-key (kbd "s-b") 'exwmx-switch-application)

    (exwmx-input-set-key (kbd "s-i") 'exwmx-sendstring-from-minibuffer)
    (exwmx-input-set-key (kbd "s-I") 'my/exwmx-sendstring)
    (exwmx-input-set-key (kbd "s-<delete>") 'exwm-logout)

    (define-key exwmx-sendstring-mode-map (kbd "C-c '") 'dingyi/exwmx-sendstring-finish)
    (define-key exwmx-sendstring-mode-map (kbd "C-c C-c") 'dingyi/exwmx-sendstring-finish)
    (define-key exwmx-sendstring-mode-map (kbd "C-c C-k") 'dingyi/exwmx-sendstring-ignore)
    ;; (define-key exwmx-sendstring-mode-map (kbd "C-c C-q") 'dingyi/exwmx-sendstring-ignore)

    ;; 没有效果啊。
    (spacemacs/set-leader-keys-for-minor-mode 'exwmx-sendstring-mode
      "m" 'dingyi/exwmx-sendstring-finish
      "k" 'dingyi/exwmx-sendstring-ignore
      "a" 'dingyi/exwmx-sendstring-ignore
      )

    ;; 加了0.2s延时，使得能在chrome上用。
    (advice-add 'exwmx-sendstring--send :override #'dingyi/exwmx-sendstring-with-delay--send)
;;;; exwmx-dmenu
    (setq exwmx-terminal-emulator "/usr/bin/terminator")

;;;; keybinding
    ;;  (exwm-input--set-key (kbd "s-;") 'ace-pinyin-dwim)
    ;; ;;;; exwm-mode-map
    (comma-def exwm-mode-map
      "t" (lambda ()
            (interactive)
            (exwm-input--fake-key ?\C-t)
            )
      "c" (lambda ()
            (interactive)
            (exwm-input--fake-key ?\C-c))
      )

    ; 根据 exwmx-appconifg来判断窗口的应用,然后通过 general的 :predicate关键字来判断是否按键定义生效，这样，就可以使得exwm不同的应用窗口，生效不同的按键。
; 比如 win+c 在 chrome窗口里是复制的意思，在其他exwm窗口里是打开chrome的意思。
; 感觉已经非常牛逼了。
  (general-def
    :keymaps 'exwm-mode-map
    ;; "s-c" (lambda ()
    ;;         (interactive)
    ;;         (if (exwm-predicate-window-by-pretty-name "terminator")
    ;;             (let ((string "copyLastCMD"))
    ;;               (dingyi/exwmx-sendstring--send 0 string)
    ;;               ;; (exwm-terminal-copy-last-command)
    ;;               (sleep-for 0.2)
    ;;               (exwm-input--fake-key ?\C-m)
    ;;               )
    ;;           (exwm-input--fake-key ?\C-c)
    ;;             )
    ;;         )
    ;; ;; "s-v" (lambda ()
    ;;         (interactive)
    ;;         (exwm-input--fake-key ?\C-v))
    ;; "s-v" (lambda ()
    ;;         (interactive)
    ;;         (exwm-input--fake-key (car (string-to-list (kbd "S-<insert>")))))
    "s-c" (lambda ()
            (interactive)
            (if (exwm-predicate-window-by-pretty-name "terminator")
                (exwm-terminal-s-c)
              (exwm-input--fake-key ?\C-c))
            )
    "s-v" 'exwm-input-fake-key--paste
    )

  (defun exwm-s-c ()
    (interactive)
    (if (exwm-predicate-window-by-pretty-name "terminator")
        (let ((string "copyLastCMD"))
          (dingyi/exwmx-sendstring--send 0 string)
          ;; (exwm-terminal-copy-last-command)
          ;; (keyboard-quit)
          ;; (sleep-for 0.1)
          ;; (general-simulate-key "C-m")
          (with-current-buffer (current-buffer)
            (exwm-input--fake-key ?\C-m)
            )
          ;; (exwm-input--fake-key (car (string-to-list (kbd "C-c"))))
          )
      (exwm-input--fake-key ?\C-c)
      )
    )
  (defun exwm-terminal-s-c ()
    (interactive)
    (exwm-input--fake-key ?\C-c)
    (dingyi/exwmx-sendstring--send 0 "copyLastCMD")
    (sleep-for 0.2)
    (exwm-input--fake-key ?\C-m)
    )
  ;; paste key
  (defun exwm-input-fake-key--paste ()
    (interactive)
    (if (derived-mode-p 'exwm-mode)
        (let ((paste-key
               (or (plist-get (exwmx-appconfig--search
                               `((:class ,exwm-class-name)
                                 (:instance ,exwm-instance-name)))
                              :paste-key)
                   exwmx-sendstring-default-paste-key)))
          ;; (sleep-for 0.2)
          (dolist (key (string-to-list (kbd paste-key)))
            (exwm-input--fake-key key)))))

  ;; 在终端里，s-c 复制 last command.
  ;; (general-def
  ;;   :keymaps 'exwm-mode-map
  ;;   ;; :predicate '(exwm-predicate-window-by-pretty-name "terminator")
  ;;   "s-c" 'exwm-terminal-copy-last-command
  ;;   )

  (defun exwm-predicate-window-by-pretty-name (pretty-name)
    (if (derived-mode-p 'exwm-mode)
        (let ((programe-name (plist-get (exwmx-appconfig--search
                                         `((:class ,exwm-class-name)
                                           (:instance ,exwm-instance-name)))
                                        :pretty-name)))
          (string= programe-name pretty-name)
          )))

  (defun exwm-terminal-copy-last-command ()
    (interactive)
    (if (derived-mode-p 'exwm-mode)
        (let ((programe-name (plist-get (exwmx-appconfig--search
                                         `((:class ,exwm-class-name)
                                           (:instance ,exwm-instance-name)))
                                        :pretty-name)))
          (if (string= programe-name "terminator")
              ;; (shell-command-to-string "fc -ln -1 | awk '{$1=$1}1'")
              ;; (exwmx-sendstring--send "fc -ln -1 | awk '{$1=$1}1' | xclip\n")
              ;; (exwmx-sendstring--send "fc -ln -1 | awk '\''{$1=$1}1'\'' ORS='\'''\'' | xclip")
              (dingyi/exwmx-sendstring--send 0 (concat "copyLastCMD"))
              ))))
  
    ;; (push (car (string-to-list (kbd "s-SPC"))) exwm-input-prefix-keys)
  
  ;; (exwm-input-set-key (kbd "s-q")
  ;;                     (lambda ()
  ;;                       (interactive)
  ;;                       (let ((buffer (or (get-buffer "Firefox") (get-buffer " Firefox"))))
  ;;                         (when buffer
  ;;                           ;; (with-selected-window (get-buffer-window buffer)
  ;;                           (with-current-buffer buffer
  ;;                             (exwm-input--fake-key ?\C-r)
  ;;                             )))
  ;;                       )
  ;;                     )
  ;; (exwm-input-set-key (kbd "s-s")
  ;;                     (lambda ()
  ;;                       (interactive)
  ;;                       (let ((buffer (or (get-buffer "Firefox") (get-buffer " Firefox"))))
  ;;                         (when buffer
  ;;                           ;; (with-selected-window (get-buffer-window buffer)
  ;;                           (with-current-buffer buffer
  ;;                             (exwm-input--fake-key ?\p))))
  ;;                       )
  ;;                     )
  ;; (exwm-input-set-key (kbd "s-q")
  ;;                     (lambda ()
  ;;                       (interactive)
  ;;                       (let ((buffer (or (get-buffer-regex *Firefox) (get-buffer-regex " Firefox"))))
  ;;                         (when buffer
  ;;                           (with-selected-window (get-buffer-window buffer)
  ;;                             (exwm-input--fake-key ?\p))))
  ;;                       )
  ;;                     )
  ;; (defun switch-to-first-matching-buffer (regex)
  ;;   (switch-to-buffer
  ;;    (car (remove-if-not (apply-partially #'string-match-p regex) (buffer-list))))
  ;;   )
  ;; (defun get-buffer-regex (regex)
  ;;   (get-buffer
  ;;    (car (remove-if-not (apply-partially #'string-match-p regex) (buffer-list)))
  ;;    ))

  ;; (get-buffer-regex "#.*Firefox")
  ;; (switch-to-first-matching-buffer "Chromium")

  ;; (defun switch-to-remote-project-buffer ()
  ;;   (interactive)
  ;;   (let ((pn (projectile-project-name)))
  ;;     (switch-to-first-matching-buffer (rx-eval `(seq bos "/ssh:" (* any) "/" ,pn eos)))))

  ;; (exwm-input-set-key (kbd "s-s")
  ;;                     (lambda ()
  ;;                       (interactive)
  ;;                       (let ((buffer (or (get-buffer "网易云音乐") (get-buffer " 网易云音乐"))))
  ;;                         (when buffer
  ;;                           (with-selected-window (get-buffer-window buffer)
  ;;                             ;; (run-at-time "0.2 sec" nil (lambda ()
  ;;                             ;;                              (exwm-input--fake-key (car (string-to-list (kbd "C-<right>"))))
  ;;                             ;;                              ))

  ;;                             (sleep-for 0.2)
  ;;                             (exwm-input--fake-key ?\C-r)
  ;;                             )
  ;;                             )))
  ;;                       )

 ;; (general-def exwm-mode-map
 ;;    "C-j" (general-simulate-key "C-<right>")
 ;;    "C-k" (general-simulate-key "C-<left>")
 ;;    )

  (general-def exwm-mode-map
    "C-j" (lambda ()
            (interactive)
            ;; (exwm-input--fake-key [C-\Right])
            (exwm-input--fake-key (car (string-to-list (kbd "C-<right>"))))
            )
    "C-k" (lambda ()
            (interactive)
            (exwm-input--fake-key (car (string-to-list (kbd "C-<left>")))))
    ;; "SPC" (lambda ()
    ;;         (interactive)
    ;;         (exwm-input--fake-key (car (string-to-list (kbd "p")))))
    )

;;;; 向exwm-buffer发送快捷键
                                        ; 通过给 general-def加关键字，使得可以在 exwm的窗口用。
  ;; (general-def
  ;;   :exwm-window "chrome"
  ;;   "t" 'c-t
  ;; )

  (exwm-input-set-key (kbd "<f1> a") 'counsel-M-x)

                                        ; 窗口判断函数
  ;; (defun exwm-chrome-p ())
  ;; (defun exwm-terminal-p ())
  ;; (defun exwm-firefox-p ())



;; (setq exwmx-programe-pretty-name (plist-get (exwmx-appconfig--search `((:class ,exwm-class-name) (:instance ,exwm-class-name))) :pretty-name))

;; (string= exwmx-programe-pretty-name "terminator")


;; (plist-get (exwmx-appconfig--search
;;             `((:class ,exwm-class-name)
;;               (:instance ,exwm-instance-name)))
;;            :pretty-name)




;;;; end
    ))

;;; minibuffer
(defun exwm/init-dingyi-minibuffer+ ()
;; (require 'dingyi-minibuffer+)
  (setq resize-mini-windows t)
  (setq max-mini-window-height 0.25)
  ;; ;; (setq exwm-workspace--attached-minibuffer-height 24)
  ;; (setq exwm-workspace-show-all-buffers nil)
  ;; (setq exwm-layout-show-all-buffers nil)
  ;; (setq exwm-workspace-minibuffer-position 'top)
  ;; (setq exwm-workspace-display-echo-area-timeout 2)
  ;; (exwm-workspace-attach-minibuffer)
  ;; (exwm-workspace-detach-minibuffer)
 )

;;; minibffer-line
;; (defun exwm/init-minibuffer-line
;;     (use-package minibuffer-line
;;       :config
;;       ;; (require 'minibuffer-line)
;;       (setq minibuffer-line-format '((:eval
;;                                       (let ((time-string (format-time-string "%l:%M %b %d %a")))
;;                                         (concat
;;                                          (make-string (- (frame-text-cols)
;;                                                          (string-width time-string)) ? )
;;                                          time-string)))))
;;       (minibuffer-line-mode)
;;       )
;;     )
;;; xbacklight 调节亮度
;; 我的组装电脑不能用。
;; xbacklight-show/decrease/increase

