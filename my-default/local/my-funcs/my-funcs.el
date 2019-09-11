;;; funcs.el --- my-default-funcs                    -*- lexical-binding: t; -*-

(defun my/copy-to-today-file ()
  (interactive)
  (if (eq major-mode 'exwm-mode)
      (exwm-input--fake-key (car (string-to-list (kbd "C-<insert>"))))
    (kill-ring-save (mark) (point))
    ;; (indicate-copied-region)
    )
  (sleep-for 0.1)
  (save-excursion
    (let ((inhibit-read-only t))
      (find-file-other-window my-today-file-path)
      ;; (deft-new-file-named (format-time-string "%Y-%m-%d"))
      (goto-char (point-max))
      (ignore-errors
        (org-meta-return))
      (let ((current-prefix-arg '(16)))
        (call-interactively 'org-time-stamp))
      (newline)
      ;;(evil-insert-state)
      (evil-paste-after 1)
      (my/remove-whole-buffer-blank-lines)
      (save-buffer)
      (quit-window)
      )
    )
  )

(defun my/read-only-my-today-file ()
  (interactive)
  (find-file-read-only my-today-file-path))

(defun my/remove-blank-lines (beg end)
  "delete empty lines if selected, otherwise the whole buffer."
  (interactive "r*")
  (let ((empty-line "^[\t ]*$"))
    (save-excursion
      (if (use-region-p)
          (flush-lines empty-line beg end)
        (flush-lines empty-line (point-min) (point-max))
        ))
    ))
;; (call-interactively 'flush-lines t (vector (rx "^\s-*$")))
;; (call-interactively 'flush-lines nil (vector "^\s-*$"))
;; (call-interactively 'flush-lines nil (vector "^[\t ]*$"))
;; (flush-lines "^[\t ]*$")
;; (flush-lines "^\s-*$")

(fset 'my/delete-buffer-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))

(defun my/remove-whole-buffer-blank-lines ()
  (interactive "*")
  (let ((empty-line "^[\t ]*$"))
    (save-excursion
      (flush-lines empty-line (point-min) (point-max))))
  )

(defun remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))

(provide 'funcs.el)

;; (defun fullscreen ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;; 	    		               '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;; (defun my/maxmized (&optional f)
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;; 	    		               '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;; 	    		               '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defvar dotspacemacs-distinguish-gui-tab nil
  "If non nil, distinguish C-i and tab in the GUI version of
emacs.")

(defvar dotspacemacs-distinguish-gui-ret nil
  "If non nil, distinguish C-m and return in the GUI version of
emacs.")

(defvar dotspacemacs-distinguish-gui-esc nil
  "If non nil, distinguish C-] and escape in the GUI version of
emacs.")

(defun spacemacs/translate-C-i (_)
  "If `dotspacemacs-distinguish-gui-tab' is non nil, the raw key
sequence does not include <tab> or <kp-tab>, and we are in the
gui, translate to [C-i]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and (not (cl-position 'tab (this-single-command-raw-keys)))
           (not (cl-position 'kp-tab (this-single-command-raw-keys)))
           dotspacemacs-distinguish-gui-tab
           (display-graphic-p))
      [C-i] [?\C-i]))
(define-key key-translation-map [?\C-i] 'spacemacs/translate-C-i)

(defun spacemacs/translate-C-m (_)
  "If `dotspacemacs-distinguish-gui-ret' is non nil, the raw key
sequence does not include <ret>, and we are in the gui, translate
to [C-m]. Otherwise, [9] (TAB)."
  (interactive)
  (if (and
       (not (cl-position 'return (this-single-command-raw-keys)))
       (not (cl-position 'kp-enter (this-single-command-raw-keys)))
       dotspacemacs-distinguish-gui-ret
       (display-graphic-p))
    [C-m] [?\C-m]))
(define-key key-translation-map [?\C-m] 'spacemacs/translate-C-m)

;; (defun spacemacs/translate-C-\[ (_)
;;   "If `dotspacemacs-distinguish-gui-esc' is non nil, the raw key
;; sequence does not include <tab> or <kp-tab>, and we are in the
;; gui, translate to [C-i]. Otherwise, [9] (TAB)."
;;   (interactive)
;;   (if (and (not (cl-position 'esc (this-single-command-raw-keys)))
;;            (not (cl-position 'kp-escape (this-single-command-raw-keys)))
;;            dotspacemacs-distinguish-gui-esc
;;            (display-graphic-p))
;;      ;; (kbd "<C-[>") [?\C-\[]
;;       [C-\[] [?\C-\[]
;;       ))
;; ;; (define-key key-translation-map [?\C-\[] 'spacemacs/translate-C-\[)
;; (define-key input-decode-map [?\C-\[] 'spacemacs/translate-C-\[)

(defun my/switch-major-mode ()
  "Switch major mode."
  (interactive)
  (minibuffer-with-setup-hook #'beginning-of-line
    (counsel-M-x " -mode$")))
;;; 激活 evil 的 yy dd 命令可以复制剪贴板
;; (fset 'evil-visual-update-x-selection 'ignore)
;; (setq mouse-drag-copy-region t)
;; ;; disable yy dd auto copy
;; ;; primary复制使能,禁用，会导致evil的yy,dd复制
;; (setq x-select-enable-primary nil)
;; ;; clipboard复制使能
;; (setq x-select-enable-clipboard nil)
;; 上面这两个只要有一个为t,就可以让goldendict,yy复制直接查词

(defun my/toggle-evil-auto-copy ()
  (interactive)
  (if (symbol-value 'x-select-enable-clipboard)
      (progn
        (setq x-select-enable-clipboard nil)
        (setq x-select-enable-primary nil)
        )
    (progn
      (setq x-select-enable-clipboard t)
      (setq x-select-enable-primary t)
      )
    )
  )

;; (defun spacemacs/dump-emacs ()
;;   "Dump emacs in a subprocess."
;;   (interactive)
;;   (when spacemacs-dump-process
;;     (message "Cancel running dumping process to start a new one.")
;;     (delete-process spacemacs-dump-process)
;;     (with-current-buffer spacemacs-dump-buffer-name
;;       (erase-buffer)))
;;   (make-directory spacemacs-dump-directory t)
;;   (setq spacemacs-dump-process
;;         (make-process
;;          :name "spacemacs-dumper"
;;          :buffer spacemacs-dump-buffer-name
;;          :command
;;          (list dotspacemacs-emacs-pdumper-executable-file
;;                "--batch"
;;                "-l" (concat user-emacs-directory "dump-init.el")
;;                "-eval" (concat "(dump-emacs-portable \""
;;                                (concat spacemacs-dump-directory
;;                                        dotspacemacs-emacs-dumper-dump-file)
;;                                "\")")))))

;;; other-window 如果 minibuffer-window 激活就选择 minibuffer
(defun other-windows-2 ()
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (other-window 1)
    )
  )

;;; no-confirms 宏,类似 ignore errors.
(defmacro no-confirm (&rest body)
  "不需要确认"
  `(cl-flet ((always-yes (&rest _) t))
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
               ((symbol-function 'yes-or-no-p) #'always-yes))
       (progn ,@body)
       )
    )
  )

;; (cl-flet ((always-yes (&rest _) t))
;;   (defun no-confirm (fun &rest args)
;;     "Apply FUN to ARGS, skipping user confirmations."
;;     (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
;;               ((symbol-function 'yes-or-no-p) #'always-yes))
;;       (apply fun args))))
;;; counsel-ag
(cl-defun counsel-ag-this-file (&optional initial-input initial-directory)
  "只搜索当前文件,接受可选参数设置初始搜索值"
  (interactive)
  (let ((initial-directory (or initial-directory default-directory)))
  (counsel-ag initial-input initial-directory (format "-G %s" (file-name-nondirectory (buffer-file-name))))))

(defun counsel-ag-search-heading-in-this-org-file ()
  "搜索当前 org 文件里的 heading, 可只搜索任意级"
  (interactive)
  (counsel-ag-this-file "^\\*{1}[^*] ")
  ;; (counsel-ag-this-file "^\\*{2}[^*] ")
  ;; (counsel-ag-this-file "^\\*{3}[^*] ")
  ;; (counsel-ag-this-file "^\\*{1,3}[^*] ")
  )

(defun counsel-ag-search-heading-in-org-directory ()
  (interactive)
  (counsel-ag "^\\*{1}[^*] " my-org-directory)
  ;; (counsel-ag "^\\*{2}[^*] " my-org-directory)
  ;; (counsel-ag "^\\*{3}[^*] " my-org-directory)
  ;; (counsel-ag "^\\*{1,3}[^*] " my-org-directory)
  )

(defun counsel-ag-search-heading-in-outshine-mode-files ()
  (interactive)
  (counsel-ag "^;{3,9}[^;] " "~/.txnix/spacemacs" "--hidden -i")
  )

(defun counsel-ag-search-heading-in-this-outshine-file ()
  (interactive)
  (counsel-ag "^;{3,9}[^;] " default-directory (format "--hidden -i -G %s" (file-name-nondirectory (buffer-file-name)))))

;;; my-funcs.el ends here
(provide 'my-funcs)
