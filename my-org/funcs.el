(defun my/helm-locate-scan-list ()
  "检测我的文件夹里的文件，只要org文件"
  (cl-loop for dir in my-org-directory
           with load-suffixes = '(".org")
           when (file-directory-p dir)
           append (directory-files
                   dir t (concat (regexp-opt (get-load-suffixes))
                                 "\\'"))))

(defun my/helm-locate-org-directories ()
  "在我的文件夹里用locate搜索所有org文件"
  (interactive)
  (helm :sources (helm-build-in-buffer-source  "Elisp libraries (Scan)"
                   :data #'my/helm-locate-scan-list
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

(defun my/deft-find-file (file)
  "Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'."
  (interactive
   (list (completing-read "Deft find file: " (deft-find-all-files-no-prefix))))
  (let* ((dir (expand-file-name deft-directory)))
    ;; If missing, add full deft-directory prefix back
    (unless (string-match (concat "^" dir) file)
      (setq file (concat dir "/" file)))
    (my/deft-open-file file)))

(defun my/deft-open-file (file &optional other switch)
  "Open FILE in a new buffer and setting its mode.
When OTHER is non-nil, open the file in another window.  When
OTHER and SWITCH are both non-nil, switch to the other window.
FILE must be a relative or absolute path, with extension."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (hack-local-variables)
      (when deft-filter-regexp
        (goto-char (point-min))
        (re-search-forward (deft-filter-regexp-as-regexp) nil t))
      ;; Ensure that Deft has been initialized
      (when (not (get-buffer deft-buffer))
        (with-current-buffer (get-buffer-create deft-buffer)
          (deft-mode)))
      ;; Set up auto save hooks
      ;; (add-to-list 'deft-auto-save-buffers buffer)
      (add-hook 'after-save-hook
                (lambda () (save-excursion
                             (deft-cache-update-file buffer-file-name)
                             (if (deft-buffer-visible-p)
                                 (deft-refresh-filter)
                               (setq deft-pending-updates t))))
                nil t)
      (run-hooks 'deft-open-file-hook))
    (if other
        (if switch
            (switch-to-buffer-other-window buffer)
          (display-buffer buffer other))
      (switch-to-buffer buffer))))

(defun emacs/find-today-file ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      ;; (find-file-other-window my-today-file-path)
      (find-file my-today-file-path)
      ;; (deft-new-file-named (format-time-string "%Y-%m-%d"))
      (goto-char (point-max))
      (ignore-errors
        ;; (org-meta-return)
        ;; C-RET比M-RET好,如果上面是list就不能新建headline了
        (org-insert-heading-respect-content)
        )
      ;; (let ((current-prefix-arg '(16)))
      ;;   (call-interactively 'org-time-stamp))
      (insert (shell-command-to-string "echo -n $(date +%T)"))
      (goto-char (point-max))
      ;; (newline)
      ;;(evil-insert-state)
      ;; (evil-paste-after 1)
      ;; (evil-insert-state)
      (save-buffer)
      ;; (quit-window)
      )
    )
  )
