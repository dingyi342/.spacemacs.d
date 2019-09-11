;;; funcs.el --- 个人用的包，一些增强的函数          -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  <dingyi@dingyi>
;; Keywords: abbrev, calendar


;; 默认与当前buffer进行diff
;; 并进入diff buffer
(defun diff-buffer-with-current-file (&optional buffer)
  "View the differences between BUFFER and its associated file.
This requires the external program `diff' to be in your `exec-path'."
  ;;(interactive "bBuffer: ") ;;这个会让你选择buffer
  (interactive)  ;;用这个默认就是使用当前buffer
  (with-current-buffer (get-buffer (or buffer (current-buffer)))
    (diff buffer-file-name (current-buffer) nil 'noasync))
  (switch-to-buffer-other-window "*Diff*")
  )



;; Use ediff instead of diff in `save-some-buffers'
(defun save-some-buffers-with-ediff ()
  (interactive)
  (eval-after-load "files"
    '(progn
       (setcdr (assq ?d save-some-buffers-action-alist)
               `(,(lambda (buf)
                    (if (null (buffer-file-name buf))
                        (message "Not applicable: no file")
                      (add-hook 'ediff-after-quit-hook-internal
                                'my-save-some-buffers-with-ediff-quit t)
                      (save-excursion
                        (set-buffer buf)
                        (let ((enable-recursive-minibuffers t))
                          (ediff-current-file)
                          (recursive-edit))))
                    ;; Return nil to ask about BUF again.
                    nil)
                 ,(purecopy "view changes in this buffer")))
       (defun my-save-some-buffers-with-ediff-quit ()
         "Remove ourselves from the ediff quit hook, and
return to the save-some-buffers minibuffer prompt."
         (remove-hook 'ediff-after-quit-hook-internal
                      'my-save-some-buffers-with-ediff-quit)
         (exit-recursive-edit)))))


;; 这会让 ediff-quit-hook 不能用。
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))


;; 切换frame是否要装饰器
(defun toggle-frame-undecorated()
  (interactive)
  (let ((undecorated (frame-parameter nil 'undecorated)))
    (if (memq undecorated '(t))
        (set-frame-parameter nil 'undecorated nil))
    (if (memq undecorated '(nil))
        (set-frame-parameter nil 'undecorated t))))


(defun dingyi/helm-locate-scan-list ()
  "检测我的文件夹里的文件，只要org文件"
  (cl-loop for dir in dingyi-org-directory
           with load-suffixes = '(".org")
           when (file-directory-p dir)
           append (directory-files
                   dir t (concat (regexp-opt (get-load-suffixes))
                                 "\\'"))))

(defun dingyi/helm-locate-dingyi-directories ()
  "在我的文件夹里用locate搜索所有org文件"
  (interactive)
  (helm :sources (helm-build-in-buffer-source  "Elisp libraries (Scan)"
                   :data #'dingyi/helm-locate-scan-list
                   :fuzzy-match helm-locate-library-fuzzy-match
                   :keymap helm-generic-files-map
                   :search (unless helm-locate-library-fuzzy-match
                             (lambda (regexp)
                               (re-search-forward
                                (if helm-ff-transformer-show-only-basename
                                    (replace-regexp-in-string
                                     "\\`\\^" "" regexp)
                                    regexp)
                                nil t)))
                   :match-part (lambda (candidate)
                                 (if helm-ff-transformer-show-only-basename
                                     (helm-basename candidate) candidate))
                   :filter-one-by-one (lambda (c)
                                        (if helm-ff-transformer-show-only-basename
                                            (cons (helm-basename c) c) c))
                   :action (helm-actions-from-type-file))
        :ff-transformer-show-only-basename nil
        :buffer "*helm locate library*"))

(defun view-journal ()
  (interactive)
  (org-journal-new-entry t nil)
  )

(defun search-all-journals ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-journal-search)
  )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))


(defun dingyi/awesome-tray-enable ()
  (interactive)
  ;; Add update timer.
  (setq awesome-tray-timer
        (run-with-timer 0 0.5 'awesome-tray-show-info))
  (add-hook 'focus-in-hook 'awesome-tray-show-info)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (with-current-buffer (current-buffer)
                (if awesome-tray-active-p
                    (spacemacs/toggle-mode-line-off))
                )
              ))
  ; Notify user.
  (setq awesome-tray-active-p t)
  (message "Enable awesome tray.")
  )

(defun dingyi/awesome-tray-disable ()
  (interactive)
  ;; show mode line
  ;; Cancel timer.
  (when (timerp awesome-tray-timer)
    (cancel-timer awesome-tray-timer))
  (remove-hook 'focus-in-hook 'awesome-tray-show-info)
  (remove-hook 'window-configuration-change-hook
            (lambda ()
              (with-current-buffer (current-buffer)
                (if awesome-tray-active-p
                    (spacemacs/toggle-mode-line-off))
                )
              ))

  ;; Update mode-line.
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer))
  ;; Notify user.
  (setq awesome-tray-active-p nil)
  (message "Disable awesome tray.")
  )

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
         (backward-char))))
(global-set-key "ρ" 'ins-brackets)

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
         (backward-char))))
(global-set-key "φ" 'ins-parens)

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
         (backward-char))))
(global-set-key "σ" 'ins-braces)

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
         (backward-char))))
(global-set-key "θ" 'ins-quotes)

;;; exwm-ins-quotes
(defun exwm-ins-quotes ()
  (interactive)
  (cond ((eq major-mode 'exwm-mode)
         (exwmx-sendstring--send "\"\"")
         (run-at-time "0.1 sec" nil (lambda () (exwm-input--fake-key ?\C-b)))
         )))


(global-set-key "χ" 'lispy-right)
(global-set-key "η" 'lispy-left)


;;; SPC h l 默认打开package.el
(defun helm-spacemacs-help//dingyi-layer-source ()
  "Construct the helm source for the layer section."
  `((name . "Layers")
    (candidates . ,(sort (configuration-layer/get-layers-list) 'string<))
    (candidate-number-limit)
    (keymap . ,helm-spacemacs-help--layer-map)
    (action . (
               ("Open packages.el"
                . helm-spacemacs-help//layer-action-open-packages)
               ("Open funcs.el"
                . helm-spacemacs-help//layer-action-open-funcs)
               ("Open config.el"
                . helm-spacemacs-help//layer-action-open-config)
               ("Open README.org"
                . helm-spacemacs-help//layer-action-open-readme)
               ("Open layers.el"
                . helm-spacemacs-help//layer-action-open-layers)
               ("Install Layer"
                . helm-spacemacs-help//layer-action-install-layer)
               ("Open README.org (for editing)"
                . helm-spacemacs-help//layer-action-open-readme-edit)))))


(defun save-and-bury-buffer ()
  (interactive)
  (save-buffer)
  (bury-buffer)
  )

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-01-18"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
(defun xdg-open (filename)
  (interactive "fFilename: ")
  (let ((process-connection-type))
    (start-process "" nil "xdg-open" (expand-file-name filename))))

(defun find-file-auto (orig-fun &rest args)
  (let ((filename (car args)))
    (if (cl-find-if
         (lambda (regexp) (string-match regexp filename))
         '("\\.pdf\\'" "\\.docx?\\'"))
        (xdg-open filename)
      (apply orig-fun args))))

(advice-add 'find-file :around 'find-file-auto)
