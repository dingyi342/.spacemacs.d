;;; funcs.el --- exwm                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  

;; Author:  <dingyi@dingyi>
;; Keywords:

(defun ag-exwm/on-exwm-edit-compose ()
  "exwm-edit hook"
  (spacemacs/toggle-visual-line-navigation-on)
  (funcall 'markdown-mode))

(defun exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (call-interactively #'exwm-input-grab-keyboard)
      (exwm-layout-show-mode-line))))

(defun exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (call-interactively #'exwm-input-release-keyboard)
      (exwm-layout-hide-mode-line))))

(defun exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (exwm-input-char-mode)
        (exwm-input-line-mode)))))

(defun evil-or-exwm-escape ()
  "escape binding to exwm or evil mode"
  (interactive)
  (with-current-buffer (window-buffer)
    (if (eq major-mode 'exwm-mode)
        (exwm-input-line-mode)
      (evil-force-normal-state))))


(defun helm-exwm-switch-terminal ()
  "切换到默认终端"
  (interactive)
  (helm-exwm-switch (file-name-nondirectory terminal-generic-program) terminal-generic-program))

(defun helm-exwm-switch-music ()
  (interactive)
  (helm-exwm-switch "crx_dkldghbjfphncdddjphinpedhldcemop" music-generic-program))

(defun dingyi/exwm-edit--finish ()
  "原来的会关闭以前的窗口，修改一下Called when done editing buffer created by `exwm-edit--compose'."
  (interactive)
  (run-hooks 'exwm-edit-before-finish-hook)
  (mark-whole-buffer)
  (kill-region (region-beginning)
               (region-end))
  ;; (kill-buffer-and-window)
  ;; 这里一定要kill掉这个buffer，否则会导致不能复制到原位置，估计是扰乱了exwm-edit--last-exwm-buffer的值？
  (quit-window t)
  (let ((buffer (switch-to-buffer exwm-edit--last-exwm-buffer)))
    (with-current-buffer buffer
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      ; 原来是 0.05s，现在改成 0.2s,本来在chrome上半有效半没效的估计就是这个延时。
      (run-at-time "0.2 sec" nil (lambda () (exwm-input--fake-key ?\C-v)))
      (setq exwm-edit--last-exwm-buffer nil))))


(defun dingyi/exwm-edit--cancel ()
  "Called to cancell editing in a buffer created by `exwm-edit--compose'."
  (interactive)
  (run-hooks 'exwm-edit-before-cancel-hook)
  ;; (kill-buffer-and-window)
  (quit-window t)
  (let ((buffer (switch-to-buffer exwm-edit--last-exwm-buffer)))
    (with-current-buffer buffer
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
   e  (exwm-input--fake-key 'right)
      (setq exwm-edit--last-exwm-buffer nil))))

(defun dingyi/exwmx-sendstring-with-delay--send (string)
  "Send `string' to clipboard and then send paste key to
    application to trigger paste operation, `string' will be
    inserted into the application.添加延时，否则在chromium里无效"
  (if (derived-mode-p 'exwm-mode)
      (let ((paste-key
             (or (plist-get (exwmx-appconfig--search
                             `((:class ,exwm-class-name)
                               (:instance ,exwm-instance-name)))
                            :paste-key)
                 exwmx-sendstring-default-paste-key)))
        (kill-new string)
        (dolist (key (string-to-list (kbd paste-key)))
          (sleep-for 0.2)
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))

(defun dingyi/exwmx-sendstring--send (delay string)
  "Send `string' to clipboard and then send paste key to
    application to trigger paste operation, `string' will be
    inserted into the application."
  (if (derived-mode-p 'exwm-mode)
      (let ((paste-key
             (or (plist-get (exwmx-appconfig--search
                             `((:class ,exwm-class-name)
                               (:instance ,exwm-instance-name)))
                            :paste-key)
                 exwmx-sendstring-default-paste-key)))
        (kill-new string)
        (dolist (key (string-to-list (kbd paste-key)))
          (sleep-for delay)
          (exwm-input--fake-key key))
        (setq kill-ring (cdr kill-ring)))
    (insert string)))



(defun my/exwmx-sendstring ()
  "Pop up a buffer and let user input, edit and send string to application."
  (interactive)
  (let ((buffer (get-buffer-create exwmx-sendstring-buffer)))
    (with-current-buffer buffer
      (markdown-mode)
      (exwmx-sendstring-mode)
      (erase-buffer)
      (setq header-line-format
            (substitute-command-keys
             (concat
              "\\<exwmx-sendstring-mode-map>"
              "Sendstring: "
              "Finish with `\\[exwmx-sendstring-finish]', "
              "Ignore with `\\[exwmx-sendstring-ignore]'. "))))
    (pop-to-buffer buffer)))


(defun dingyi/exwmx-sendstring-finish ()
  "Send the string in buffer and delete window."
  (interactive)
  (if exwmx-sendstring-mode
      (let ((string (buffer-string)))
        (quit-window t)
        ;; (kill-buffer exwmx-sendstring-buffer)
        (exwmx-sendstring--send string))
    (message "EXWM-X: exwmx-sendstring-mode is not enabled.")))

(defun dingyi/exwmx-sendstring-ignore ()
  "Ignore send string to application."
  (interactive)
  (if exwmx-sendstring-mode
      (progn
        (quit-window t)
        ;; (kill-buffer exwmx-sendstring-buffer))
        (message "EXWM-X: exwmx-sendstring-mode is not enabled."))))


(defun exwm-logout ()
  (interactive)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "lxsession-logout"))


;; Can be used to bind a key to jumping to an application, or alternatively starting it.  E.g.:
;;
;; (spacemacs/exwm-bind-switch-to-or-run-command "s-f" "Firefox" "firefox")
;;
;; The window class can be found out with exwm's builtin info functions, but for most applications it should just match the buffer name.
(defun spacemacs/exwm-bind-switch-to-or-run-command (key window-class command)
  (exwm-input-set-key (kbd key)
                      `(lambda ()
                         (interactive)
                         (spacemacs/exwm-switch-to-buffer-or-run ,window-class ,command))))

(defun spacemacs//exwm-switch-to-line-mode ()
  "Used as a hook to switch to line mode when transient mode starts."
  (when (eq exwm--input-mode 'char-mode)
    ;; (setq exwm--switch-to-char-after-transient (current-buffer))
    (call-interactively 'exwm-input--grab-keyboard)))

(defun spacemacs//exwm-persp-mode-inhibit-p (frame)
  (frame-parameter frame 'unsplittable))

(defun spacemacs/exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(defun spacemacs/exwm-switch-to-buffer-or-run (window-class command)
  "Switch to first buffer with window-class, and if not present, run command."
  (let ((buffer
         (find window-class (buffer-list) :key (lambda(b) (cdr (assoc 'exwm-class-name (buffer-local-variables b)))) :test 'string-equal)))
    (if buffer
        (exwm-workspace-switch-to-buffer buffer)
      (start-process-shell-command command nil command))))

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(defun spacemacs/exwm-rename-buffer ()
  (let* ((part1 exwm-class-name)
         (part2 (when (not (string-equal exwm-class-name exwm-title))
                  (concat "/" exwm-title)))
         (name (concat part1 (or part2 "")))
         (maxlen 40))
    (exwm-workspace-rename-buffer (if (> (length name) maxlen)
                                      (concat (subseq name 0 (- maxlen 3)) "...")
                                    name))))

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

(defun spacemacs/exwm-layout-toggle-fullscreen ()
  "Togggles full screen for Emacs and X windows"
  (interactive)
  (if exwm--id
      (if exwm--fullscreen
          (exwm-reset)
        (exwm-layout-set-fullscreen))
    (spacemacs/toggle-maximize-buffer)))

(defun spacemacs/exwm-run-program-in-home (command)
  (let ((default-directory user-home-directory))
    (start-process-shell-command command nil command)))

(defun spacemacs/exwm-app-launcher (command)
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ivy"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-launch-split-below (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-below-and-focus)
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-launch-split-right (command)
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (split-window-right-and-focus)
  (spacemacs/exwm-run-program-in-home command))

(defun spacemacs/exwm-jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))

(defun spacemacs/exwm-exwm-buffers-info ()
  "Helper, return information about open exwm windows"
  (loop for buffer in (buffer-list)
        for name = (buffer-name buffer)
        for ecname = (buffer-local-value 'exwm-class-name buffer)
        when ecname
        collect (list :buffer-name name :exwm-class-name ecname)))


(defun exwm-C-c ()
  (interactive)
  (exwm-input--fake-key ?\C-c))
(defun exwm-C-t ()
  (interactive)
  (exwm-input--fake-key ?\C-t))
;; (defun exwm-C-g ()
;; (interactive)
;; (exwm-input--fake-key (car (string-to-list (kbd "<escape>")))))


(defun exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (call-interactively #'exwm-input-grab-keyboard)
  (exwm-layout-show-mode-line))

(defun exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line"
  (call-interactively #'exwm-input-release-keyboard)
  (exwm-layout-hide-mode-line))

(defun exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (exwm-input-char-mode)
        (exwm-input-line-mode)))))


;;; 调节亮度
(defun dingyi/brightness-adjust-by-race (race)
  "set brightness to value with xrandr by race"
  (let*  ((current-brightness (string-to-number (shell-command-to-string (format "xrandr --current --verbose | grep -m 1 'Brightness:' | cut -f2- -d:"))))
          (set-brightness (+ current-brightness race))
          )
    (if (and
         (not (> set-brightness 1.0))
         (not (< set-brightness 0.0))
         )
        (progn
          (shell-command-to-string (format "xrandr --output HDMI-1 --brightness %s" set-brightness))
          (message "current brightness is %s" set-brightness)
          )
      (message "number overflow")
      )
    )
  )

(defun dingyi/brightness-adjust-increase-slowly ()
  (interactive)
  (dingyi/brightness-adjust-by-race +0.05))
(defun dingyi/brightness-adjust-increase-fastly ()
  (interactive)
  (dingyi/brightness-adjust-by-race +0.10))
(defun dingyi/brightness-adjust-decrease-slowly ()
  (interactive)
  (dingyi/brightness-adjust-by-race -0.05))
(defun dingyi/brightness-adjust-decrease-fastly ()
  (interactive)
  (dingyi/brightness-adjust-by-race -0.10))

(defun dingyi/exwmx-button-enable ()
  "Enable exwmx-button."
  (interactive)
  (add-hook 'exwm-update-class-hook #'exwmx-button--update-button-line)
  (add-hook 'exwm-update-title-hook #'exwmx-button--update-button-line)
  (add-hook 'buffer-list-update-hook #'exwmx-button--update-button-line))


(defun exwmx-button-disable ()
  "Disable exwmx-button."
  (interactive)
  (remove-hook 'exwm-update-class-hook #'exwmx-button--update-button-line)
  (remove-hook 'exwm-update-title-hook #'exwmx-button--update-button-line)
  (remove-hook 'buffer-list-update-hook #'exwmx-button--update-button-line))


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



;; (defun my/exwm-switch-firefox ()
;;   (interactive)
;;   (helm-exwm-switch "firefox" "/usr/bin/firefox" )
;;   )

;; (defun my/exwm-switch-chromium ()
;;   (interactive)
;;   (helm-exwm-switch "chromium" "/usr/bin/chromium")
;;   )

;; (defun my/exwm-switch-dolphin ()
;;   (interactive)
;;   (helm-exwm-switch "dolphin" "/usr/bin/dolphin")
;;   )

;; ;; (mapcar (lambda (app)
;;           (let ((app-fn-name (intern (concat "my/exwm-switch2" app))))
;;             `(defun ,app-fn-name ()
;;                (interactive)
;;                (helm-exwm-switch ,app (executable-find ,app)))
;;             ))
;;         (list ("emacs" "firefox" "dolfpin" "chromium")))

;; (mapc
;;  (defalias (intern (concat "my/exwm-switch-" app))
;;    (lambda (app)
;;      (interactive)
;;      (helm-exwm-switch app (executable-find app))
;;      )
;;    )
;;  (list ("emacs" "firefox" "dolfpin" "chromium")))

;; (dolist (app '("emacs" "firefox" "dolfpin" "chromium"))
;;   (defalias (intern (concat "my/exwm-switch" app))
;;     (lambda (app)
;;       (interactive)
;;       (helm-exwm-switch app (executable-find app))))
;;   )

; How can I create a multiple defuns by looping through a list?

(defun my/gen-exwm-app-fn (app)
  (let ((app-fn-name (intern (format "my/exwm-switch-%s" app))))
    `(defun ,app-fn-name ()
       (interactive)
       (require 'helm-exwm)
       (helm-exwm-switch ,app (executable-find ,app)))))

(defmacro my/gen-exwm-app-funs ()
  `(progn ,@(mapcar
             (lambda (x)
               (my/gen-exwm-app-fn x))
             my/exwm-app)))

;; (macroexpand '(my/gen-all-app-funs))


;; (exwm-input-set-key (kbd "s-m")
    ;;                     (lambda ()
    ;;                       (interactive)
    ;;                       ;; (switch-to-buffer (helm-exwm-switch-music))
    ;;                       (exwmx-quickrun "163music" t)
    ;;                       ))

    ;; (exwm-input-set-key (kbd "s-m")
    ;;                     (lambda ()
    ;;                       (interactive)
    ;;                       ;; (switch-to-buffer (helm-exwm-switch-music))
    ;;                       (exwmx-quickrun "163music" t)
    ;;                       ))



(defalias 'quit-everything 'my-exwm-default-layout)
(defun my-exwm-default-layout ()
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
    ;; 如果同时开太多新窗口的话，就好多执行几次了。
    (if (> (count-windows) 2)
        (winner-undo))
    ;; quit recursive
    (ignore-errors
      (abort-recursive-edit) ;C-]
      (keyboard-escape-quit) ;M-ESC ESC
      (keyboard-quit)        ;C-g
      )
    ;; (sleep-for 3)
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer)
      (force-window-update))
    (exwmx-floating-hide-all)
    (with-current-buffer (current-buffer)
      (exwm-floating-hide)
      )
    )
  )



(defun exwm-switch-to-1-workspace ()
  (interactive)
  (exwm-workspace-switch 0))

(defun exwm-switch-to-2-workspace ()
  (interactive)
  (exwm-workspace-switch 1))

(defun exwm-switch-to-3-workspace ()
  (interactive)
  (exwm-workspace-switch 2))

(defun exwm-switch-to-4-workspace ()
  (interactive)
  (exwm-workspace-switch 3))

