
;;; switch 最近的buffer
(defun switch-to-nth-buffer (n)
  "Switches to nth most recent buffer. Ignores a bunch of stuff."
  (catch 'tag
    (mapcar (lambda (b)
              (unless
                  (or
                   ;; (and (not switch-include-erc) (eq (buffer-local-value 'major-mode b) 'erc-mode))
                   (minibufferp b)
                   (string-match "^ " (buffer-name b))
                   (equal b (current-buffer)))
                (if (= n 1)
                    (progn
                      (switch-to-buffer b)
                      (throw 'tag nil))
                  (setq n (- n 1)))))
            (buffer-list))))

(defun switch-to-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 1))
(defun switch-to-second-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 2))
(defun switch-to-third-most-recent-buffer ()
  (interactive)
  (switch-to-nth-buffer 3))

;; save-and-magit-diff-unstaged-buffer
(defun my/save-and-magit-unstaged-current-buffer ()
  (interactive)
  (if (and (eq lsp-mode t) (eq major-mode 'java-mode))
      (lsp-format-buffer)
    )
  (save-buffer)
  ;; (magit-diff-unstaged)
  (magit-diff-unstaged nil (list (magit-file-relative-name)))
  ;; (magit-diff-setup-buffer nil nil (magit-diff-arguments) (magit-current-file))
  ;; 如过 buffer 的内容超过了 buffer 大小就最大化
  ;; (if (> (point-max) 640)
  ;;     (spacemacs/maximize-vertically)
  ;;   )
  ;; 根据高度自动 shrink
  (magit-section-show-level-3-all)
  (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
  ;; (let ((window-min-height (/ (frame-height) 2)))
  ;;   (fit-window-to-buffer)
  ;;   )
  )

;(nmap "M-s" 'my/save-and-magit-unstaged-current-buffer)
;(imap "M-s" 'my/save-and-magit-unstaged-current-buffer)

;; 显示所有 unstaged, 同时只展开当前文件的 section.
;; 保存所有未保存的文件, 同时 unstaged.
;; 啥?
(defun my/save-all-and-magit-unstaged-buffer ()
  (interactive)
  ;; format-buffer
  (if (and (eq lsp-mode t) (eq major-mode 'java-mode))
      (lsp-format-buffer)
    )
  (evil-write-all nil)
  (magit-diff-unstaged)
  (magit-section-show-level-3-all)
  (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
  )

;(nmap "M-S" 'my/save-all-and-magit-unstaged-buffer)

(defun my/magit-diff-buffer-file ()
  (interactive)
  ;; (add-hook 'magit-diff-mode-hook (lambda ()
  ;;                                   (if magit-current-popup
  ;;                                       (magit-section-show-level-2-all))))
  ;; (advice-add 'magit-diff-buffer-file :after 'magit-section-show-level-2-all)
  (magit-diff-buffer-file)
  ;; (sleep-for 0.5)
  ;; (magit-section-show-level-2-all)
  ;; (run-with-idle-timer 0.5 nil 'magit-section-show-level-2-all)
  ;; (if magit-current-popup
  ;;     (magit-section-show-level-2-all)
  ;;     )
  ;; (advice-remove 'magit-diff-buffer-file 'magit-section-show-level-2-all)
  (run-with-idle-timer 0.01 nil
                       (lambda ()
                         (magit-section-show-level-2-all)
                         (fit-window-to-buffer (selected-window) (frame-height) (/ (frame-height) 2))
                         (beginning-of-buffer)
                         ))
  ;; (magit-maybe-set-dedicated)
  ;; (set-window-dedicated-p (selected-window))
  (spacemacs/toggle-current-window-dedication)
  ;; (remove-hook 'magit-diff-mode-hook (lambda ()
  ;;                                   (if magit-current-popup
  ;;                                       (magit-section-show-level-2-all))))
  )

;(nmap "g:" 'my/magit-diff-buffer-file)

