;;; packages.el --- my-switch-buffer layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: dingyi <dingyi@dingyi>
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
;; added to `my-switch-buffer-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-switch-buffer/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-switch-buffer/pre-init-PACKAGE' and/or
;;   `my-switch-buffer/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-switch-buffer-packages
  '(
    iflipb
    ibuffer-projectile
    ibuffer-vc
    (awesome-tab :location (recipe :fetcher github
                                   :repo "manateelazycat/awesome-tab"))
    )
  )

;;; iflipb
(defun my-switch-buffer/init-iflipb ()
  (use-package iflipb
    :config
    ;; 循环
    ;; 不使用循环，给自己干扰
    ;; 如果 iflipb 里没有有用的buffer，开启循环，还会切换到其他废buffer
    ;; (setq iflipb-wrap-around nil)
    (setq iflipb-wrap-around t)

    ;; (defun get-buffers-matching-mode (mode)
    ;;   "Returns a list of buffers where their major-mode is equal to MODE"
    ;;   (let ((buffer-mode-matches '()))
    ;;  (dolist (buf (buffer-list))
    ;;    (with-current-buffer buf
    ;;      (if (eq mode major-mode)
    ;;    (add-to-list 'buffer-mode-matches buf))))
    ;;  buffer-mode-matches))

;;;; ignore buffers
    ;; ;; 忽略的buffer
    ;; "^.*\.org$" 忽略所有org文件,
    (setq iflipb-ignore-modes '(
                                magit-mode
                                magit-status-mode
                                magit-process-mode
                                magit-diff-mode
                                dired-mode
                                ))
    (setq iflipb-ignore-buffers
          '(
            "^[*]"                      ;忽略以*开头的buffer, *message*
            "^[#]"
            "^[\[]"                     ;忽略以[开头的buffer，主要是Exwm buffer
            "^.*\.jpg$"                 ;忽略jpg图片buffer
            ;; "^.*\.org$"                 ;忽略org文件buffer
            "^.*\.png$"                 ;忽略png图片buffer
            "^[^.]+$"                   ;忽略不含.的buffer，主要是dired的buffer
            "^.*\.zip$"
            ;; 忽略dired的buffer
            ;; 会传递buffer给函数，如果该函数值为t，则excluded这个buffer.
            (lambda (b)
              (with-current-buffer b
                (eq 'dired-mode major-mode)))

            (lambda (b)
              (with-current-buffer b
                (memq major-mode iflipb-ignore-modes)))

            ;; 隐藏所有 org-agenda files
            (lambda (b)
              (with-current-buffer b
                (member (buffer-file-name) (org-agenda-files))
                )
              )

            )
          )
;;;; ivy-iflipb buffer list
    (defun ivy-iflipb ()
      (interactive)
      (ivy-read "iflipb : " (my/iflipb-buffer-list)
                :action (lambda (f)
                          (with-ivy-window
                            (switch-to-buffer f)))
                :require-match t
                :caller 'counsel-recentf))

    (defun iflipb-make-buffer-list (default)
      (let* (
             (ido-ignore-buffers iflipb-ignore-buffers)
             (ido-current-buffers (ido-get-buffers-in-frames 'current))
             (ido-temp-list (ido-make-buffer-list-1 (selected-frame) ido-current-buffers)))
        (if ido-temp-list
            (nconc ido-temp-list ido-current-buffers)
          (setq ido-temp-list ido-current-buffers))
        ido-temp-list)
      )

    (defun my/iflipb-buffer-list ()
      "Return the current list of buffers.
      Currently visible buffers are put at the end of the list.
      See `ido-make-buffer-list' for more infos."
      (require 'ido)
      (let ((ido-process-ignore-lists t)
            ido-ignored-list
            (ido-use-virtual-buffers nil)
            )
        (iflipb-make-buffer-list nil)))
    (my/iflipb-buffer-list)

;;;; hydra
    (spacemacs|define-transient-state iflipb
      :title "iflipb Transient State"
      :doc "
 Tab^^                    Group^^                   Other^^
 ───────^^────────────  ─────^^───────────────  ─────^^──────────────
 [_j_/_k_] pre/next       [_p_/_n_] pre/next group  [_,_/_._] other pre/next
 [_1_/_2_] beginning/end  [_s_] switch            [_d_/_3_] delete/Kill all    [_q_] quit"
      ;; :on-enter (awesome-tab-toggle-tabbar-mode-on)
      ;; :on-exit (awesome-tab-toggle-tabbar-mode-off)
      :bindings
      ;; Tab
      ("," iflipb-previous-buffer)
      ("." iflipb-next-buffer)
      ("n" iflipb-next-buffer)
      ("p" iflipb-previous-buffer)
      ("j" iflipb-next-buffer)
      ("k" iflipb-previous-buffer)

      ("1" switch-to-most-recent-buffer)
      ("2" switch-to-second-most-recent-buffer)
      ("3" switch-to-third-most-recent-buffer)
      ("d" spacemacs/kill-this-buffer)
      ("<tab" iflipb-next-buffer)
      ("s" ivy-switch-buffer)
      ("q" nil :exit t))

;;;; 定义快捷键
    ;; :general
    ;; (general-def
    ;;   :states '(normal evilified)
    ;;   :keymaps 'override
    ;;   "M-h" 'iflipb-next-buffer
    ;;   "M-H" 'iflipb-next-buffer
    ;;   "M-l" 'iflipb-previous-buffer
    ;;   "M-k" 'spacemacs/kill-this-buffer
    ;;   ;; "M-s" 'evil-write-all             ;保存所有
    ;;   "M-s" 'my/save-and-magit-unstaged-buffer
    ;;   "M-o" 'other-window
    ;;   "M-j" 'ivy-iflipb
    ;;   "M-u" 'winner-undo
    ;;   "M-d" 'bury-buffer
    ;;   )
    ;; (general-def
    ;;   :states '(insert emacs)
    ;;   :keymaps 'override
    ;;   "M-s" 'my/save-and-magit-unstaged-buffer
    ;;   )

    ;; (comma-def
    ;;   ;; "o," 'switch-bury-or-kill-buffer
    ;;   "n" 'iflipb-next-buffer
    ;;   "," 'iflipb-next-buffer
    ;;   "." 'iflipb-previous-buffer
    ;;   "p" 'iflipb-previous-buffer
    ;;   "<tab>" 'spacemacs/alternate-buffer
    ;;   "1" 'switch-to-most-recent-buffer
    ;;   "2" 'switch-to-second-most-recent-buffer
    ;;   "3" 'switch-to-third-most-recent-buffer
    ;;   )
    ;; (general-def
    ;;   :states '(evilified motion)
    ;;   :keymaps 'override
    ;;   :prefix ","
    ;;   "n" 'iflipb-next-buffer
    ;;   "," 'iflipb-next-buffer
    ;;   "." 'iflipb-previous-buffer
    ;;   "p" 'iflipb-previous-buffer
    ;;   "<tab>" 'spacemacs/alternate-buffer
    ;;   "1" 'switch-to-most-recent-buffer
    ;;   "2" 'switch-to-second-most-recent-buffer
    ;;   "3" 'switch-to-third-most-recent-buffer
    ;;   )
    )
  )

;;; ibuffer-projectile
(defun my-switch-buffer/post-init-ibuffer-projectile ()
  (use-package ibuffer-projectile
    :config
    ;; 手动
    ;; M-x ibuffer-project-set-filter-groups
    ;; 打开ibuffer自动激活
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))
    ;; To display filenames relative to the projectile root, use project-relative-file in ibuffer-formats.
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 18 18 :left :elide)
                  " "
                  (size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  project-relative-file)))
    ;; ibuffer-projectile-filter-groups 获取列表，可以自己自由组合。
    )
  )

;;; ibuffer-vc
(defun my-switch-buffer/init-ibuffer-vc ()
  (use-package ibuffer-vc
    :config
    )
  )

;;; awesome-tab
(defun my-switch-buffer/init-awesome-tab ()
  (use-package awesome-tab
    :commands (awesome-tab-toggle-tabbar-mode-on awesome-tab-toggle-tabbar-mode-off)
    ;; :defer (spacemacs/defer)
    :init
    (awesome-tab-mode 1)
;;;; hydra1
    (progn
      (spacemacs|add-toggle awesome-tab
        :status (awesome-tab-mode)
        :on (awesome-tab-mode)
        :off (awesome-tab-mode -1)
        :documentation "Toggle the visibility of modeline."
        :evil-leader "tmB"
        )
      (spacemacs|define-transient-state awesometab
        :title "Awesome-tab Transient State"
        :doc "
 Tab^^                    Group^^                   Other^^
 ───────^^────────────  ─────^^───────────────  ─────^^──────────────
 [_h_/_l_] pre/next       [_p_/_n_] pre/next group  [_H_/_L_] other pre/next
 [_b_/_e_] beginning/end  [_s_] switch            [_d_/_K_] delete/Kill all    [_q_] quit"
        :on-enter (spacemacs/toggle-awesome-tab-on)
        :on-exit (spacemacs/toggle-awesome-tab-off)
        :bindings
        ;; Tab
        ("h" awesome-tab-backward)
        ("l" awesome-tab-forward)
        ("b" awesome-tab-select-beg-tab)
        ("e" awesome-tab-select-end-tab)
        ;; Group
        ("p" awesome-tab-backward-group)
        ("n" awesome-tab-forward-group)
        ("s" awesome-tab-switch-group)
        ;; Other
        ("d" spacemacs/kill-this-buffer)
        ("K" awesome-tab-kill-all-buffers-in-current-group)
        ("H" awesome-tab-forward-tab-other-window)
        ("L" awesome-tab-backward-tab-other-window)
        ("q" nil :exit t))

      ;; (general-def "M-t" 'spacemacs/awesometab-transient-state/body)

;;;; hydra2
      (defhydra awesome-fast-switch (:hint nil
                                           ;; :pre (spacemacs/toggle-awesome-tab-on)
                                           ;; :post (spacemacs/toggle-awesome-tab-off)
                                           )
        "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _a_/_C-a_^^     select first | _b_ search buffer | _d_/_C-k_   kill buffer
 _h_   _l_  switch tab   | _e_/_C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _f_/_C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
        ("h" awesome-tab-backward-tab)
        ("j" awesome-tab-forward-group)
        ("k" awesome-tab-backward-group)
        ("l" awesome-tab-forward-tab)
        ("0" my-select-window)
        ("1" my-select-window)
        ("2" my-select-window)
        ("3" my-select-window)
        ("4" my-select-window)
        ("5" my-select-window)
        ("6" my-select-window)
        ("7" my-select-window)
        ("8" my-select-window)
        ("9" my-select-window)
        ("a" awesome-tab-select-beg-tab)
        ("C-a" awesome-tab-select-beg-tab)
        ("A" awesome-tab-move-current-tab-to-beg)
        ("e" awesome-tab-select-end-tab)
        ("C-e" awesome-tab-select-end-tab)
        ("f" awesome-tab-ace-jump)
        ("C-j" awesome-tab-ace-jump)
        ("C-h" awesome-tab-move-current-tab-to-left)
        ("C-l" awesome-tab-move-current-tab-to-right)
        ("b" ivy-switch-buffer)
        ("g" awesome-tab-counsel-switch-group)
        ("d" spacemacs/kill-this-buffer)
        ("C-k" kill-current-buffer)
        ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
        ("K" awesome-tab-kill-all-buffers-in-current-group)
        ("H" awesome-tab-forward-tab-other-window)
        ("L" awesome-tab-backward-tab-other-window)
        ("q" nil "quit"))

      (general-def "M-j" 'awesome-fast-switch/body)
      )
;;;; cofig
    :config
    (awesome-tab-mode 1)
;;;; select window by number
    ;; winum users can use `winum-select-window-by-number' directly.
    (defun my-select-window-by-number (win-id)
      "Use `ace-window' to select the window by using window index.
WIN-ID : Window index."
      (let ((wnd (nth (- win-id 1) (aw-window-list))))
        (if wnd
            (aw-switch-to-window wnd)
          (message "No such window."))))

    (defun my-select-window ()
      (interactive)
      (let* ((event last-input-event)
             (key (make-vector 1 event))
             (key-desc (key-description key)))
        (my-select-window-by-number
         (string-to-number (car (nreverse (split-string key-desc "-")))))))

;;;; ,1 select visible tab
    (comma-def
      "1" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 1))
      "2" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 2))
      "3" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 3))
      "4" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 4))
      "5" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 5))
      "6" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 6))
      "7" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 7))
      "8" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 8))
      "9" (lambda () (interactive) (awesome-tab-select-visible-nth-tab 9))
      )
;;;; helm resouce
    (awesome-tab-build-helm-source)
    (defun helm-awesome-tab ()
      (interactive)
      (awesome-tab-mode 1)
      (helm :sources 'helm-source-awesome-tab-group
            :buffer "*helm awesometab*"))
;;;; 设置 tab 外观
    (setq awesome-tab-style 'bar)
    ;; (setq awesome-tab-style "box")

    (setq awesome-tab-height 36)
    ;; (setq awesome-tab-label-fixed-length 14)

    ;; (progn
    ;;   (setq awesome-tab-active-color
    ;;         (face-attribute 'spacemacs-normal-face :background)
    ;;         awesome-tab-inactive-color
    ;;         (face-attribute 'font-lock-comment-face :foreground))
    ;;   (set-face-attribute 'awesome-tab-selected nil
    ;;                       :foreground awesome-tab-active-color
    ;;                       :underline nil
    ;;                       :overline awesome-tab-active-color
    ;;                       )
    ;;   (set-face-attribute 'awesome-tab-unselected nil
    ;;                       :foreground awesome-tab-inactive-color
    ;;                       :underline awesome-tab-inactive-color
    ;;                       :overline nil
    ;;                       ))

;;;; 在 which-key 里禁用 tabbar
    ;; (add-hook 'which-key-mode-hook (lambda ()
    ;;                                  (interactive)
    ;;                                  (awesome-tab-mode -1)))
    ;; (add-hook 'which-key-init-buffer-hook (lambda ()
    ;;                                         (interactive)
    ;;                                         (awesome-tab-mode -1)))

    ;; 设置勾子不行,看下面放到 awesome-tab-hide-tab 函数里
    ;; (push 'which-key-mode-hook awesometab-hide-tabs-hooks)
    ;; (push 'treemacs-mode-hook awesometab-hide-tabs-hooks)
    ;; (add-hook 'treemacs-mode-hook '(lambda () (setq-local header-line-format nil)))

;;;; hide rules, 返回为 t, 就隐藏该 buffer
    (defun awesome-tab-hide-tab (x)
      (let ((name (format "%s" x)))
        (or
         (string-prefix-p "*epc" name)
         (string-prefix-p "*helm" name)
         (string-prefix-p "*Compile-Log*" name)
         (string-prefix-p "*lsp" name)
         (string-prefix-p " *which-key*" name)
         (and (string-prefix-p "magit" name)
              (not (file-name-extension name)))
         ;; 不要 agenda-files 文件
         ;; 这里要用 funcall
         (member (funcall 'buffer-file-name x) (org-agenda-files))
         )))
;;;; 设置分组规则
    (defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ;; ((derived-mode-p 'eshell-mode)
     ;; "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((memq major-mode '(vterm-mode eshell-mode shell-mode term-mode))
     "TermShell")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

;;;; 快捷键定位某个分组
    (nmap "M-1" (lambda () (interactive) (awesome-tab-switch-group "OrgMode")))
    (nmap "M-2" (lambda () (interactive) (awesome-tab-switch-group "Elisp")))
    (nmap "M-3" (lambda () (interactive) (awesome-tab-switch-group "Dired")))
    ;; (nmap "M-4" (lambda () (interactive) (awesome-tab-switch-group "OrgMode")))
    ;; (nmap "M-v" (lambda () (interactive) (awesome-tab-switch-group "TermShell")))
;;;; 结束
    )
  )

;;; packages.el ends here
