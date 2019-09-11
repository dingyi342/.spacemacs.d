;;; packages.el --- dingyi layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <dingyi@dingyi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;;; package
(setq dingyi-packages
     '(
       ;; general
       (dingyi-keybindings :location local)
       electric-operator                ;操作符中间自动增加空格
       diff
       ediff
       ;; (thing-edit :location (recipe :fetcher github
                                     ;; :repo "manateelazycat/thing-edit"))
       ;; (aria2 :location (recipe :fetcher github
                                ;; :repo "dingyi342/aria2"))
      ;; (embrace :excluded t)
       ;; evil-embrace
       ;; (pacman :location local)
       ;; arch-packer
       org
       ;; (fuo :location (recipe :fetcher github
                              ;; :repo "dingyi342/emacs-fuo"))
       org-journal
       calendar
       ;; (awesome-tab :location (recipe :fetcher github
                                      ;; :repo "manateelazycat/awesome-tab"))
       anki-editor
       ;; (company-english-helper :location (recipe :fetcher github
                                                 ;; :repo "manateelazycat/company-english-helper"))
       ;; (awesome-tray :location (recipe :fetcher github
                                       ;; :repo "manateelazycat/awesome-tray"))
       ;; (awesome-pair :excluded t :location (recipe :fetcher github
                                       ;; :repo "manateelazycat/awesome-pair"))

       evil-find-char-pinyin
       evil-snipe
       ;; (nes :excluded t :location (recipe :fetcher github
                              ;; :repo "gongo/emacs-nes"))
       ;; (mapleline :location local)
       ;; (maple-preview :location (recipe :fetcher github
                                        ;; :repo "honmaple/emacs-maple-preview"))
       lispy
       iflipb
       ;; (ms-python  :requires lsp-mode
       ;;             :location (recipe
       ;;                        :fetcher github
       ;;                        :repo "xhcoding/ms-python"))
       helm
       helm-spacemacs-help
       ;; engine-mode
       ;; (meme :excluded t :location (recipe :fetcher github
                               ;; :repo "larsmagne/meme"))
       evil-collection
       (emacsql :location (recipe :fetcher github
                                  :repo "skeeto/emacsql"))
       (org-db :location local)
       beacon
       popup-imenu
       ;; origami
       ;; package ends.
       ))
;;; general

;; (defun dingyi/init-general ()
;;   (require 'general)
;;   (general-evil-setup)
;;   (general-evil-setup t)
;;   (general-create-definer comma-def
;;     :states '(normal)
;;     :prefix ","
;;     :prefix-command 'comma-prefix-command
;;     :prefix-map 'comma-prefix-map
;;     )
;;   )

;;; dingyi-keybindings
(defun dingyi/init-dingyi-keybindings ()

;;;; 按键表示
;; (kbd)
;; [C-tab]
;; S-<insert>
;; S-<spc>
;;;; general definer
  ;需要放在 bootstrap里面。

  ;; (general-create-definer comma-def
  ;;   :states 'normal
  ;;   :keymaps 'override
  ;;   :prefix ","
  ;;   :prefix-command 'comma-prefix-command
  ;;   :prefix-map 'comma-prefix-map
  ;;   )

  ;; (general-create-definer comma-def
  ;;   :states '(normal evilified)
  ;;   :prefix ","
  ;;   )

  ;; (general-create-definer semicolon-def
  ;;   :states 'normal
  ;;   :keymaps 'override
  ;;   :prefix ";"
  ;;   :prefix-command 'semicolon-prefix-command
  ;;   :prefix-map 'semicolon-prefix-map
  ;;   )

  ;; (general-create-definer semicolon-def
  ;;   :states '(normal evilified)
  ;;   :prefix ";")

;;;; semicolon-def
  ;; (semicolon-def
  ;;   "k" 'spacemacs/kill-this-buffer
  ;;   "f" 'ace-pinyin-dwim
  ;;   "q" 'spacemacs/kill-this-buffer
  ;;   "s" 'save-buffer
  ;;   "e" 'ediff-current-file
  ;;   "j" 'ace-pinyin-dwim
  ;;   "x" 'counsel-M-x
  ;;   )

;;;; comma-def
  (comma-def
    "a" 'org-agenda
    "b" 'helm-mini
    "b" (general-key-dispatch 'helm-mini
          :timeout 0.3
          "m" 'emms)
    ;; "c" 'org-capture
    "c" 'dingyi/capture-to-deft-today-file
    "d" 'save-and-bury-buffer
    "e" 'ediff-current-file
    ;; "f" 'dingyi/helm-locate-dingyi-directories
    "f" (general-key-dispatch 'counsel-find-file
          :timeout 0.2
         "f" 'dingyi/helm-locate-dingyi-directories)
    "g" (general-key-dispatch 'magit-status
          :timeout 0.3
          "c" 'magit-clone
          "m" 'magit-dispatch-popup
          "t" 'spacemacs/time-machine-transient-state/body
          )
    "h" (general-key-dispatch 'helm-info-at-point
          :timeout 0.3
          "f" 'describe-function
          "k" 'describe-bindings
          "c" 'describe-char
          "F" 'describe-face
          "p" 'describe-package
          "v" 'describe-variable)
    "i" (general-key-dispatch 'insert-char
          :timeout 0.3
          "t" 'spacemacs/timestamp-transient-state/body
          "d" 'help/insert-datestamp
          "f" 'cnfonts-insert-fontname
          "y" 'yas-insert-snippet
          )
    "j" (general-key-dispatch
            'org-db-open-heading
         ;; 'helm-org-agenda-files-headings
         ;; 'counsel-org-agenda-headlines
          :timeout 0.3
          "b" 'lazy-helm/spacemacs/helm-jump-in-buffer
          "j" 'oi-jump
          "m" 'helm-imenu
          "l" 'org-db-open-link-in-file
          "f" 'org-db-open-file
          "r" 'org-db-open-recent-file
          )
    "k" 'spacemacs/kill-this-buffer
    "q" 'kill-buffer-and-window
    "r" (general-key-dispatch 'helm-org-rifle
          :timeout 0.3
          "b" 'helm-org-rifle-current-buffer
          "d" 'helm-org-rifle-directories
          "o" 'helm-org-rifle-org-directory
          "a" 'helm-org-rifle-agenda-files
          )
    "s" (general-key-dispatch 'helm-swoop
           :timeout 0.2
           "b" 'helm-chrome-bookmarks
           "g" 'engine/search-google
           "t" 'goldendict-dwim
           "m" 'fuo-search
           )
    "t" 'counsel-M-x
    ;; (lookup-key spacemacs-default-map (kbd "f"))
    "SPC" (copy-keymap spacemacs-default-map)
    )
;;;; helm,ivy
  (general-def "M-x" 'counsel-M-x)
  (general-def "C-x C-f" 'counsel-find-file)
  (spacemacs/set-leader-keys "ff" 'counsel-find-file)
  ;; (general-def
  ;;   "C-h" (general-key-dispatch 'evil-delete-backward-word
  ;;           :timeout 0.2
  ;;           "f" 'counsel-describe-function
  ;;           "v" 'counsel-describe-variable
  ;;           "k" 'describe-key
  ;;           )
  ;;   )
;;;; org-mode-map
  (comma-def org-mode-map
    "j" (general-key-dispatch
          ;; 'helm-org-agenda-files-headings
          'org-db-open-heading
          :timeout 0.3
          "b" 'lazy-helm/spacemacs/helm-jump-in-buffer
          ;"j" 'oi-jump
          "m" 'helm-imenu
          ;;添加的
          "j" 'spacemacs/helm-jump-in-buffer
          ))

  ;; (general-def
  ;;   :states '(normal evilifed visual insert emacs)
  ;;   :keymaps 'override
  ;;   "M-h" (general-key-dispatch (lambda ()
  ;;                                 (interactive)
  ;;                                 (let ((map (format-message "%s-map" major-mode)))
  ;;                                 (lookup-key org-mode-map (kbd "M-h"))
  ;;                                 ))
  ;;           :timeout 0.3
  ;;           "b" 'counsel-M-x)
  ;;   "M-j" (general-key-dispatch 'org-metadown
  ;;           :timeout 0.3
  ;;           "j" 'ace-pinyin-dwim
  ;;           "f" 'ace-pinyin-jump-char-in-line
  ;;           "u" 'spacemacs/avy-goto-url
  ;;           "w" 'avy-goto-word-or-subword-1
  ;;           "l" 'avy-goto-line)
  ;;   )

  (general-def
    :states '(normal motion evilifed visual insert emacs)
    :keymaps 'override
    "M-h" (general-key-dispatch 'org-metaleft
            :timeout 0.3
            "b" 'counsel-M-x)
    "M-j" (general-key-dispatch 'org-metadown
            :timeout 0.3
            "j" 'ace-pinyin-dwim
            "f" 'ace-pinyin-jump-char-in-line
            "u" 'spacemacs/avy-goto-url
            "w" 'avy-goto-word-or-subword-1
            "l" 'avy-goto-line)
    )

;;;; outshine-mode-mape
;outshine-mode-map比 prog-mode-map更好一点。
  (comma-def outshine-mode-map
    "j" (general-key-dispatch
            'org-db-open-heading
          ;; 'helm-org-agenda-files-headings
          :timeout 0.3
          "b" 'lazy-helm/spacemacs/helm-jump-in-buffer
          ;"j" 'oi-jump
          "m" 'helm-imenu
          ;;添加的
          "j" 'oi-jump
          "n" 'helm-navi-headings
          "N" 'helm-navi
          ))

;;;; prog-mode-map
  ;; (comma-def prog-mode-map
  ;;   "j" (general-key-dispatch 'helm-org-agenda-files-headings
  ;;         :timeout 0.3
  ;;         "j" 'oi-jump
  ;;        ))

;;;; 单键修饰键

;;;; w
  (general-def
    :states '(normal evilified motion)
    "w" (general-key-dispatch 'evil-forward-word-begin
          :timeout 0.1
          "h" 'evil-window-left
          "j" 'evil-window-down
          "k" 'evil-window-up
          "l" 'evil-window-right))

;;;; a
  (general-def
    :states '(normal evilified motion)
    "a" (general-key-dispatch 'evil-append
          :timeout 0.1
          "j" 'counsel-M-x
          "k" 'helm-M-x
          "n" 'isearch-forward
          "l" 'pinyin-search
          ))
  (general-def
    :states '(insert emacs)
    "a" (general-key-dispatch 'self-insert-command
          :timeout 0.1
          "j" 'counsel-M-x
          "k" 'helm-M-x
          ;; "n" 'isearch-forward  ;;an英文单词里出现的频率还是很高的。
          ;; "l" 'pinyin-search
          ))
;;;; f

  (general-def
    :states '(normal evilified motion)
    "f" (general-key-dispatch 'evil-find-char-pinyin
          :timeout 0.1
          "h" 'evil-delete-backward-word
          ;; "j" 'evil-ret
          "j" (general-simulate-key "RET")
          ;; "k" (general-simulate-key "M-RET")
          "l" 'evil-open-below
          "k" 'evil-delete-backward-char-and-join
          ;; ";" '
          ))
  (general-def
    :states '(insert emacs)
    "f" (general-key-dispatch 'self-insert-command
          :timeout 0.1
          "h" 'evil-delete-backward-word
          "j" 'evil-ret
          ;; "k" (general-simulate-key "M-RET")
          ;; "k" 'org-meta-return
          "k" (lambda () (interactive) (evil-open-below 1) (org-meta-return))
          "l" 'evil-open-below
          ))

;;;; ;
  (general-def
    :states '(normal evilified motion)
    ";" (general-key-dispatch 'evil-repeat-find-char
          :timeout 0.2
          "s" 'save-buffer
          "k" 'spacemacs/kill-this-buffer
          "f" 'ace-pinyin-dwim
          "q" 'spacemacs/kill-this-buffer
          "s" 'save-buffer
          "e" 'ediff-current-file
          "j" 'ace-pinyin-dwim
          ))
  (general-def
    :keymaps '(counsel-find-file-map helm-find-files-map)
    ";" (general-key-dispatch 'self-insert-command
          :timeout 0.3
          "t" (lambda () (interactive) (insert "~/"))
          "f" (lambda () (interactive) (insert "~/OneDrive/org"))
          "m" (lambda () (interactive) (insert "/mnt/e/musics"))
          "v" (lambda () (interactive) (insert "/mnt/e/03videos"))
          "b" (lambda () (interactive) (insert "/mnt/e/02books"))
          "i" (lambda () (interactive) (insert "/mnt/e/01images"))
          ))
  (general-def
    :keymaps '(counsel-find-file-map helm-find-files-map)
    "/" (general-key-dispatch 'self-insert-command
          :timeout 0.75
          "t" (lambda () (interactive) (insert "~/"))
          "f" (lambda () (interactive) (insert "~/OneDrive/org/"))
          "m" (lambda () (interactive) (insert "/mnt/e/musics/"))
          "v" (lambda () (interactive) (insert "/mnt/e/03videos/"))
          "b" (lambda () (interactive) (insert "/mnt/e/02books/"))
          "i" (lambda () (interactive) (insert "/mnt/e/01images/"))
          ))
  (general-def "C-x C-f" 'counsel-find-file)
  (general-def
    :states '(insert emacs)
    ";" (general-key-dispatch 'toggle-input-method
          :timeout 0.3
          ;;"s" 'save-buffer
          "q" (general-simulate-key "θ")
          ;; "w" (general-simulate-key "\\")
          "w"  (lambda () (interactive) (insert "\\"))
          ;; "e" (general-simulate-key "=")
          "e"  (lambda () (interactive) (insert "="))
          "r" (general-simulate-key "ρ")
          ;; "t" (general-simulate-key "~")
          "t"  (lambda () (interactive) (insert "~"))
          ;; ---
          "y" (general-simulate-key "υ")
          "u" (general-simulate-key "δ")
          "i" (general-simulate-key "TAB")
          "o" (general-simulate-key "DEL")
          "p" (general-simulate-key "π")
          ;; ---
          ;; "a" (general-simulate-key "-")
          "a"  (lambda () (interactive) (insert "-"))
          ;; "s" (general-simulate-key "_")
          "s"  (lambda () (interactive) (insert "_"))
          ;; "d" (general-simulate-key ":")
          "d"  (lambda () (interactive) (insert ":"))
          "f" (general-simulate-key "φ")
          ;; "g" (general-simulate-key ">")
          "g"  (lambda () (interactive) (insert ">"))
          ;; ---
          "h" (general-simulate-key "η")
          ;; "j" (general-simulate-key ";")
          "j" (lambda () (interactive) (insert ";"))
          "k" (general-simulate-key "κ")
          ;; "l" (general-simulate-key "<")
          "l"  (lambda () (interactive) (insert "<"))
          ;; ---
          ;; "z" (general-simulate-key "+")
          "z"  (lambda () (interactive) (insert "+"))
          "x" (general-simulate-key "χ")
          "c" (general-simulate-key "σ")
          "v" (general-simulate-key "RET")
          "b" (general-simulate-key "β")
          "n" (general-simulate-key "ν")
          "m" (general-simulate-key "μ")
          ;; "1" (general-simulate-key "!")
          ;; "1" (exwm-input--fake-key (car (string-to-list (kbd "!"))))
          ))

  (general-def
    :states '(insert emacs)
    "；" (general-key-dispatch 'toggle-input-method
          :timeout 0.2
          "s" 'save-buffer
          "j" (lambda () (interactive) (insert "；"))
          ))

;;;; ,
  (general-def
    :states '(insert emacs)
    "," (general-key-dispatch 'self-insert-command
          :timeout 0.25
          "g" 'magit-status
          ))
;;;; helm-org-headings-map
  ;; 在helm-org里，让 Enter选择 C-c i 打开 indirect
  (general-def helm-org-headings-map "RET" "C-c i")

;;;; motion evilified
  (general-def
    :states '(evilified motion)
    :keymaps 'override
    :prefix ","
    "k" 'spacemacs/kill-this-buffer
    "q" 'kill-buffer-and-window
    "d" 'bury-buffer
    "," 'iflipb-next-buffer
    "." 'iflipb-previous-buffer
    )

  ;; (general-def
  ;;   :states '(evilified motion)
  ;;   :prefix ","
  ;;   "x" 'counsel-M-x)
  (general-def
    "s-v" (general-simulate-key "S-<insert>"))

;;;; fn leader

;;;; org-capture-minor-mode
  (comma-def
    :definer 'minor-mode
    :keymaps 'org-capture-mode
    "k" (general-simulate-key "C-c C-k")
    ;; "c" "C-c C-c"
    "c" (general-simulate-key "C-c C-c")
    )

;;;; 一些默认设置
  ;; 把 s 变成 avy-goto-char
  (general-nvmap :keymaps 'override "s" 'avy-goto-char-2)
  ;; ;; modeline显示文件大小。
  (size-indication-mode)
  ;; SPC T fspacemacs/toggle-fringe
  ;;(spacemacs/toggle-fringe-on)

  (fringe-mode)
  (setq version-control-diff-side 'left)

  (add-hook 'org-mode-hook 'fringe-mode)

  ;;设置为nil,就不会抖动了。
  (setq resize-mini-windows nil)

  (setq font-name "ConsolasWithYahei" font-size 12)

  (defun set-frame-font-size (&optional font-size)
    "Sets font size for all frames. Default is font-size"
    (interactive (list
                  (read-number "number: " font-size)))
    (let ((font-size (or font-size font-size)))
      (set-frame-font (format "%s %d" font-name font-size) nil t)))

  ;; (set-frame-font-size)
;;;; 显示行号
  (global-linum-mode 0);太卡了。
  (global-display-line-numbers-mode)
;;;; use evil everywhere
  ;; 这样就不要在纠结键的绑定问题了。
  ;; 应该放在 dotspacemacs/user-init里。
  ;; (setq evil-emacs-state-modes nil)
  ;; (setq evil-insert-state-modes nil)
  ;; (setq evil-motion-state-modes nil)
  ;; (setq evil-evilified-state-modes nil)
;;;; end
  )

;;; electric-operator
(defun dingyi/init-electric-operator ()
  (use-package electric-operator
    :init
    ))

;;; post diff
(defun dingyi/post-init-diff ()
  :post-config
  (spacemacs/set-leader-keys
    "od" 'diff-buffer-with-current-file)
  )
;;; post ediff
(defun dingyi/post-init-ediff ()
  :post-config
  (comma-def
    "D" 'ediff-current-file)
  (add-hook 'after-init-hook #'save-some-buffers-with-ediff)
  )

;;; thing-edit
(defun dingyi/init-thing-edit ()
  " 1. 用expand region来选择,SPC v 很方便。
    2. evil text object已经支持了，d/y/c<test obeject>就对应着cut/copy/replace,有点重复了，参考一下增加一些文本对象：sexp,email,filename,url,word,symbol,defun,list,sentence,whitespace,page,line,comment,paragrap,parentheses,所谓的提醒范围的功能，可以用 v<test-object>d/y/c"
  (use-package thing-edit
    :config
    ;;email
    (define-key evil-inner-text-objects-map
      "de" 'thing-cut-email)
    (define-key evil-inner-text-objects-map
      "ye" 'thing-copy-email)
    (define-key evil-inner-text-objects-map
      "ce" 'thing-cut-email)
    ;;sexp
    (define-key evil-inner-text-objects-map
      "ds" 'thing-cut-sexp)
    (define-key evil-inner-text-objects-map
      "ys" 'thing-copy-sexp)
    (define-key evil-inner-text-objects-map
      "cs" 'thing-cut-sexp)
    ;;filename
    (define-key evil-inner-text-objects-map
      "dF" 'thing-cut-filename)
    (define-key evil-inner-text-objects-map
      "yF" 'thing-copy-filename)
    (define-key evil-inner-text-objects-map
      "cF" 'thing-cut-filename)
    ;;defun
    (define-key evil-inner-text-objects-map
      "df" 'thing-cut-defun)
    (define-key evil-inner-text-objects-map
      "yf" 'thing-copy-defun)
    (define-key evil-inner-text-objects-map
      "cf" 'thing-cut-defun)
    ;;list
    (define-key evil-inner-text-objects-map
      "dl" 'thing-cut-list)
    (define-key evil-inner-text-objects-map
      "yl" 'thing-copy-list)
    (define-key evil-inner-text-objects-map
      "cl" 'thing-cut-list)
    ;;paratheses
    (define-key evil-inner-text-objects-map
      "dp" 'thing-cut-parentheses)
    (define-key evil-inner-text-objects-map
      "yp" 'thing-cut-parentheses)
    (define-key evil-inner-text-objects-map
      "cp" 'thing-cut-parentheses)
    ;;comment
    (define-key evil-inner-text-objects-map
      "dc" 'thing-cut-comment)
    (define-key evil-inner-text-objects-map
      "yc" 'thing-copy-comment)
    (define-key evil-inner-text-objects-map
      "cc" 'thing-cut-comment)

    ;; (define-key evil-inner-text-objects-map
    ;;   "de" 'thing-cut-email)
    ;; (define-key evil-inner-text-objects-map
    ;;   "ye" 'thing-copy-email)
    ;; (define-key evil-inner-text-objects-map
    ;;   "ce" 'thing-cut-email)

    ))

;;; aria2
(defun dingyi/init-aria2 ()
  (use-package aria2
    ))

;; (defun dingyi/init-emojify ()
;;   (use-package emojify
;;     :init
;;     (add-hook 'after-init-hook #'global-emojify-mode)
;;     (setq org-bullets-bullet-list '(":whale:" ":whale2:" ":dragon:" ":tiger:"))
;;     ))

;; (custom-set-faces
;;  '(org-bullet-face ((t (:foreground “burlywood” :weight normal :height 1.5)))))
;; (setq org-bullets-face-name (quote org-bullets-face))
;; (set-face-attribute 'org-bullet-face
;;                     t :foreground "burlywood" :weight 'normal :height 1.6)
;; (custom-set-faces
;;  '(org-bullet-face ((t (:foreground "burlywood" :weight normal :height 1.5)))))

;; (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
;; (setq org-bullets-bullet-list '("♠" "♣" "♥" "♦" "♤" "♧" "♡" "♢"))
;; (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
;; (setq org-bullets-bullet-list '("◐" "◑" "◒" "◓" "◴" "◵" "◶" "◷" "⚆" "⚇" "⚈" "⚉" "♁" "⊖" "⊗" "⊘"))
;; (setq org-bullets-bullet-list '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦" "◯" "⚪" "⚫" "⚬" "❍" "￮" "⊙" "⊚" "⊛" "∙" "∘"))
;; (setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))

;; (setq org-bullets-bullet-list '("☀" "♼" "☼" "☾" "☽" "☣" "§" "¶" "‡" "※" "✕" "△" "◇" "▶" "◀" "◈"))

;; (set-fontset-font "fontset-default" nil
;;                   (font-spec :size 20 :name "Symbola"))

;;spacemacs 已经内置了 SPC w t
;; (defun toggle-window-dedicated ()
;;   "Control whether or not Emacs is allowed to display another
;; buffer in current window."
;;   (interactive)
;;   (message
;;    (if (let (window (get-buffer-window (current-buffer)))
;;          (set-window-dedicated-p window (not (window-dedicated-p window))))
;;        "%s: Can't touch this!"
;;      "%s is up for grabs.")
;;    (current-buffer)))

;; (global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;;; embrace
(defun dingyi/init-embrace ()
  "非evil用户用的，evil-surround"
  (use-package embrace
    ))

;;; evil-embrace
(defun dingyi/init-evil-embrace ()
  "evil-surround 来处理定义好的 text-object,如果没有text-object定义surrounding paris,就不能处理了，需要自己定义text-object?比如 \textbf{ }这样用text-object不适合，用evil-embrace.
  现在evil-surround也可以定义了自己的 paris或着直接定义 text-object"
  (use-package evil-embrace
    :config
    ;; enable evil-surround integration
    (evil-embrace-enable-evil-surround-integration)
    ;; (evil-embrace-disable-evil-surround-integration)

    ;; evil-surround处理的keys，定义在下面的变量里,其他的都由evil-surround来处理
    (setq-default evil-embrace-evil-surround-keys '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?< ?> ?b ?B ?t ?q ?m))
    ;; 该变量是 buffer-local的，所以可以 change it in the hook:
    ;; (add-hook 'LaTeX-mode-hook
              ;; (lambda ()
                ;; (add-to-list 'evil-embrace-evil-surround-keys ?o)))

    ;;help message popup烦人
    ;; (setq evil-embrace-show-help-p nil)

    ;; org-mode 定义的一些pairs
    (add-hook 'org-mode-hook 'embrace-org-mode-hook)

    ;;定义自己的 pairs in embrace

    ))

;;; pacman
(defun dingyi/init-pacman ()
  "https://github.com/brbetances/conf-scripts/blob/master/emacs-dir/custom-modes/pacman.el"
  (use-package pacman

    ))

;;; arch packer

(defun dingyi/init-arch-packer ()
  "pacman或AUR(pacaur)的前端，问题太多了。"
  (use-package arch-packer
    :config
    ;; 默认改为 pacaur
    ;; (setq arch-packer-default-command "pacaur")

    ;; 使用其他 AUR helper
    ))

;;; post org

(defun dingyi/post-init-org ()
  (comma-def org-mode-map
    ;; "c" 'org-capture
    "c" 'dingyi/capture-to-deft-today-file
    "t" 'helm-imenu)
  (setq org-src-window-setup 'current-window)
  (with-eval-after-load 'helm-locate
    (comma-def "f" 'dingyi/helm-locate-dingyi-directories)
    ;; (general-def global-map :prefix "," "f" 'dingyi/helm-locate-dingyi-directories)
    )
  )

;;; fuo

(defun dingyi/init-fuo ()
  (use-package fuo
    ;;开机自动开岂feelown服务
    ;; (comma-def :infix "s" "m" 'fuo-search)
    ;;增加helm接口
    ))

;; (defun dingyi/init-org-journal ()
;;   (use-package org-journal
;;     :init
;;     (progn
;;       (spacemacs/set-leader-keys
;;        "ajj" 'org-journal-new-entry
;;        "ajv" 'view-journal
;;        "ajs" 'org-journal-search
;;        "ajS" 'search-all-journals
;;        )
;;       (spacemacs/set-leader-keys-for-major-mode
;;        "jn" 'org-journal-open-next-entry
;;        "jp" 'org-journal-open-previous-entry
;;        "jj" 'org-journal-new-entry
;;        "js" 'org-journal-search)
;;       (setq org-journal-dir "~/OneDrive/org/journal/")
;;       ;; (setq spacemacs-org-journal-mode-map (copy-keymap spacemacs-org-mode-map))
;;       ;; (spacemacs//init-leader-mode-map 'org-journal-mode 'spacemacs-org-journal-mode-map)
;;       )
;;     ))

;;; post org-journal

(defun dingyi/post-init-org-journal ()
  "已经被org layer占了"
  (use-package org-journal
    :config
    (setq org-journal-dir "~/OneDrive/org/journal")
    (with-eval-after-load 'org-capture
      (add-to-list 'org-capture-templates
                   '("j" "journal entry" entry (function org-journal-find-location)
                     "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))
    (spacemacs/set-leader-keys
      "aojv" 'view-journal
      "aojS" 'search-all-journals)
    )
  )

;;; calendar

(defun dingyi/init-calendar ()
  (use-package calendar
    :config
    (progn
      (define-key calendar-mode-map "Jj" 'org-journal-new-date-entry)
      (define-key calendar-mode-map "Jv" 'org-journal-read-entry)
      (define-key calendar-mode-map "JV" 'org-journal-display-entry)
      (define-key calendar-mode-map "Jn" 'org-journal-next-entry)
      (define-key calendar-mode-map "Jp" 'org-journal-previous-entry)
      (define-key calendar-mode-map "JS" 'org-journal-search-forever)
      (define-key calendar-mode-map "Jw" 'org-journal-search-calendar-week)
      (define-key calendar-mode-map "Jm" 'org-journal-search-calendar-month)
      (define-key calendar-mode-map "Jy" 'org-journal-search-calendar-year)
      )
    ))

;;; awesome-tab

(defun dingyi/init-awesome-tab ()
  (use-package awesome-tab
    :config
    (progn
    (with-eval-after-load 'helm-buffer
     (awesome-tab-build-helm-source)
     (add-to-list 'helm-mini-default-sources 'helm-source-awesome-tab-group))
    ;; 不想默认启动
    ;; (awesome-tab-mode 1)
    ;; 配色与doom-dracula适配，只能看见当前的，看不对其他的

    )
    ))

;;; anki-editor

(defun dingyi/init-anki-editor ()
  (use-package anki-editor
    ))

;;; company-english-helper

(defun dingyi/init-company-english-helper ()
  "两个文件，一个叫company-english-helper.el是主文件，一个是company-english-helper-data.el是词库文件，都加载后，执行 toggle-company-english-helper就可以用了，write english on the fly."
  (require 'company-english-helper))

;;; awesome-tray

(defun dingyi/init-awesome-tray ()
  "还蛮好看的，spacemacs可以用toggle-mode-line来隐藏，如果要显示modeline的话有点不好看，可以在awesome-tray里设置一下不要对mode-line做修改。由用户自己决定是否显示modeline.
其次,M-x 输入的字符看不见啊。"
  (use-package awesome-tray
    :config
    ;; (setq awesome-tray-active-modules '("date"))
    ;; (dingyi/awesome-tray-enable)
    ;; (setq awesome-tray-info-padding-right 25)
    )
  ;; (require 'awesome-tray)
  ;; (awesome-tray-mode 1)
  ;; 启用了awesome-tray-mode,自动隐藏 mode-line-mode

  ;; (set (make-variable-buffer-local 'mode-line-format) nil)
  ;; (add-hook 'window-configuration-change-hook
  ;;          (lambda ()
  ;;            (with-current-buffer (current-buffer)
  ;;              (if awesome-tray-active-p
  ;;                  (spacemacs/toggle-mode-line-off))
  ;;              )
  ;;            ))

  ;; ;;;; https://www.emacswiki.org/emacs/HideModeLine
  ;; (if awesome-tray-active-p
      ;; (setq-default mode-line-format nil))
  ;; (setq mode-line-format nil)
  ;; (setq-default mode-line-format nil)

  ;; (add-hook 'awesome-tray-mode-hook 'spacemacs/toggle-mode-line-off)
  ;; 不要对modeline修改，用spacemacs的 toggle-mode-line来让用户自己选择，默认是不显示。

  ;; 修复M-x输入的字符不可见。

  )

;;; awesome-pair

(defun dingyi/init-awesome-pair ()
  (use-package awesome-pair
    :config
    (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
    (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
    (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
    (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
    (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
    (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)

    (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
    (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

    (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
    (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

    (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
    (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
    (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
    (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
    (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

    (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
    (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
    (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

    (dolist (hook (list
                   'c-mode-common-hook
                   'c-mode-hook
                   'c++-mode-hook
                   'java-mode-hook
                   'haskell-mode-hook
                   'emacs-lisp-mode-hook
                   'lisp-interaction-mode-hook
                   'lisp-mode-hook
                   'maxima-mode-hook
                   'ielm-mode-hook
                   'sh-mode-hook
                   'makefile-gmake-mode-hook
                   'php-mode-hook
                   'python-mode-hook
                   'js-mode-hook
                   'go-mode-hook
                   'qml-mode-hook
                   'jade-mode-hook
                   'css-mode-hook
                   'ruby-mode-hook
                   'coffee-mode-hook
                   'rust-mode-hook
                   'qmake-mode-hook
                   'lua-mode-hook
                   'swift-mode-hook
                   'minibuffer-inactive-mode-hook
                   ))
      (add-hook hook '(lambda () (awesome-pair-mode 1))))
    ))

;;; post evil-surround

(defun dingyi/post-init-evil-surround ()
  (use-package evil-surround
    :config
    (spacemacs|define-text-object "m" "brackets-mark" "（" "）")
    (spacemacs|define-text-object "q" "quotation-mark" "“" "”")
    (setq-default evil-surround-pairs-alist evil-surround-pairs-alist)
    )
  )

;;; evil-find-char-pinyin

(defun dingyi/init-evil-find-char-pinyin ()
  (use-package evil-find-char-pinyin
    :config
    (evil-find-char-pinyin-mode +1)
    ))

;;; evil-snipe

(defun dingyi/init-evil-snipe ()
  (use-package evil-snipe
    :config

    ))

;;; nes

(defun dingyi/init-nes ()
  (require 'nes)
  )

;;; mapeline

(defun dingyi/init-mapleline ()
  (use-package mapleline

    )
  ;; (require 'mapleline)
  )

;;; maple preview

(defun dingyi/init-maple-preview ()
  (use-package maple-preview
    ))

;;; lispy

(defun dingyi/init-lispy ()
  (use-package lispy
    :config
    (define-key lispy-mode-map (kbd "φ") 'lispy-parens)
    (define-key lispy-mode-map (kbd "σ") 'lispy-braces)
    (define-key lispy-mode-map (kbd "ρ") 'lispy-brackets)
    (define-key lispy-mode-map (kbd "θ") 'lispy-quotes)
    (define-key lispy-mode-map (kbd "χ") 'lispy-right)
    ))

;;; iflipb

(defun dingyi/init-iflipb ()
  (use-package iflipb
  :config
  (progn
    ;; 循环
    (setq iflipb-wrap-around t)
    ;; 忽略的buffer
    ;; "^.*\.org$" 忽略所有org文件,
    (setq iflipb-ignore-buffers
          '(
            "^[*]"                      ;忽略以*开头的buffer, *message*
            "^[\[]"                     ;忽略以[开头的buffer，主要是Exwm buffer
            "^.*\.jpg$"                 ;忽略jpg图片buffer
            ;; "^.*\.org$"                 ;忽略org文件buffer
            "^.*\.png$"                 ;忽略png图片buffer
            "^[^.]+$"                   ;忽略不含.的buffer，主要是dired的buffer
            ))
    ;; 定义快捷键
    (comma-def
      ;; "o," 'switch-bury-or-kill-buffer
      "n" 'iflipb-next-buffer
      "," 'iflipb-next-buffer
      "." 'iflipb-previous-buffer
      "p" 'iflipb-previous-buffer
      "<tab>" 'spacemacs/alternate-buffer
      "1" 'switch-to-most-recent-buffer
      "2" 'switch-to-second-most-recent-buffer
      "3" 'switch-to-third-most-recent-buffer
      )
    )
  )
  )

;;; ms-python

(defun dingyi/init-ms-python ()
  "这个应该也没什么用了。"
  (use-package ms-python
    :init
    (progn
      (add-hook 'python-mode-hook #'lsp)
      (setq ms-python-dir "~/python-language-server/output/bin/Release/")
      )))

;;; post helm

(defun dingyi/post-init-helm ()
  (use-package helm
    :config
    (spacemacs|define-transient-state dingyi-helm-navigation
      :title "Helm Transient State"
      :doc "
 [_j_/_k_]  next/prev candidate  [_v_]^^     persistent action     [_e_]^^    edit occurrences
 [_h_/_l_]  prev/next source     [_a_.._g_]  action 1..5         [_t_/_T_]  toggle visible/all mark
 [_q_]^^    quit                 [_1_]^^     action selection pg"
      :foreign-keys run
      :on-enter (spacemacs//helm-navigation-ts-on-enter)
      :on-exit  (spacemacs//helm-navigation-ts-on-exit)
      :bindings
      ("a" spacemacs/helm-action-1 :exit t)
      ("s" spacemacs/helm-action-2 :exit t) ("d" spacemacs/helm-action-3 :exit t) ("f" spacemacs/helm-action-4 :exit t) ("g" spacemacs/helm-action-5 :exit t) ("6" spacemacs/helm-action-6 :exit t)
      ("7" spacemacs/helm-action-7 :exit t)
      ("8" spacemacs/helm-action-8 :exit t)
      ("9" spacemacs/helm-action-9 :exit t)
      ("0" spacemacs/helm-action-10 :exit t)
      ("<tab>" helm-select-action :exit t)
      ("TAB" helm-select-action :exit t)
      ("<RET>" helm-maybe-exit-minibuffer :exit t)
      ;; ("?" nil :doc (spacemacs//helm-navigation-ts-full-doc))
      ("1" spacemacs/helm-transient-state-select-action)
      ("e" spacemacs/helm-ts-edit)
      ("5" helm-beginning-of-buffer)
      ("G" helm-end-of-buffer)
      ("h" helm-previous-source)
      ("j" helm-next-line)
      ("k" helm-previous-line)
      ("l" helm-next-source)
      ("q" nil :exit t)
      ("M-SPC" nil :exit t)
      ("t" helm-toggle-visible-mark)
      ("T" helm-toggle-all-marks)
      ("v" helm-execute-persistent-action))
    (define-key helm-map (kbd "<C-i>")
      'spacemacs/dingyi-helm-navigation-transient-state/body)
    ;; (define-key helm-map (kbd "C-z")
    ;;   'spacemacs/dingyi-helm-navigation-transient-state/body)
     )
  )

;;; post helm-spacemacs-help
(defun dingyi/post-init-helm-spacemacs-help ()
  (use-package helm-spacemacs-help
    :config
    ;; 覆盖，把package.el调到第一位。好像没什么用。
    (advice-add 'helm-spacemacs-help//layer-source :override 'helm-spacemacs-help//dingyi-layer-source)
    ))

;; (defun dingyi/post-init-engine-mode ()
;;   (comma-def
;;     :infix "s"
;;     "g" 'engine/search-google)
;;   )

;;; meme
(defun dingyi/init-meme ()
  (require 'meme)
  (autoload 'meme "meme.el" "Create a meme from a collection" t)
  (autoload 'meme-file "meme.el" "Create a meme from a file" t)
  )

;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame t
;;       helm-use-undecorated-frame-option t)

;;; evil-collection
(defun dingyi/init-evil-collection ()
  (use-package evil-collection
    :after evil
    ;;会弹出warnning.
    :init
    (defadvice display-warning
        (around no-warn-evil-collection (type message &rest unused) activate)
      "Ignore the warning about set `evil-want-keybinding' to nil"
      (unless (and
               (> 5 4)
               ;;(eq type 'evil-collection)  ;; 这个有问题。
                   (or (string-prefix-p "Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details." message t)
                       (string-prefix-p "`evil-want-keybinding' was set to nil but not before loading evil.\
\n
Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details." message t))
                   )
      ad-do-it))
    (ad-activate 'display-warning)
    ))

;;(load-file "/home/dingyi/.emacs.d/elpa/26.1/develop/evil-collection-20190219.839/evil-collection.el")

;;; emacsql
(defun dingyi/init-emacsql ()
  )
;;; org-db
(defun dingyi/init-org-db ()
  (require 'org-db))

;;; beacon
(defun dingyi/init-beacon ()
  (use-package beacon
    :config
    (beacon-mode 1)
    ;; 橘黄色跟spacemacs默认的normal小的光标颜色一样，配。
    (setq beacon-color "Orange" beacon-size 90 beacon-blink-duration 0.4 beacon-blink-delay 0.2)
    ;; 资料
    ;; https://github.com/Malabarba/beacon
    ;; 函数
    ;; beacon--dec
    ;; beacon-mode                         ;启用
    ;; beacon-blink
    ;; beacon--shine
    ;; beacon--vanish
    ;; beacon--int-range
    ;; beacon--movement->
    ;; beacon--record-vars
    ;; beacon--make-overlay
    ;; beacon--post-command
    ;; beacon-blink-automated
    ;; beacon--colored-overlay
    ;; beacon--maybe-push-mark
    ;; beacon--compilation-mode-p
    ;; beacon--after-string-overlay
    ;; beacon--visual-current-column
    ;; beacon--window-scroll-function
    ;; beacon--blink-on-focus
    ;; beacon--ov-at-point
    ;; beacon--ov-put-after-string
    ;; beacon--color-range
    ;; ;; 变量
    ;; beacon-size
    ;; beacon--ovs
    ;; beacon-mode
    ;; beacon-color
    ;; beacon--timer
    ;; beacon-lighter
    ;; beacon-push-mark
    ;; beacon-mode-hook
    ;; beacon-blink-delay
    ;; beacon-blink-duration
    ;; beacon--previous-place
    ;; beacon--previous-window
    ;; beacon-overlay-priority
    ;; beacon--window-scrolled
    ;; beacon-before-blink-hook
    ;; beacon-blink-when-focused
    ;; beacon-dont-blink-commands
    ;; beacon--previous-mark-head
    ;; beacon-dont-blink-predicates
    ;; beacon--previous-window-start
    ;; beacon-dont-blink-major-modes
    ;; beacon-blink-when-buffer-changes
    ;; beacon-blink-when-window-scrolls
    ;; beacon-blink-when-window-changes
    ;; beacon-blink-when-point-moves-vertically
    ;; beacon-blink-when-point-moves-horizontally
    ;; ;; 在指定buffer禁用
    ;; (setq-local beacon-mode nil)
    ;; ;; hook
    ;; beacon-mode-hook
    ;; beacon-before-blink-hook
    ))

;;; popup-imenu
(defun dingyi/init-popup-imenu ()
  (use-package popup-imenu
    :config
    (setq popup-imenu-fuzzy-match t
          popup-imenu-style 'indent     ;'flat 'indent indent是使用空格来缩进层级
          popup-imenu-position 'point   ;在光标处显示 'fill-column 'center
          imenu-use-popup-menu 'on-mouse      ;nil 总是使用minibuffer prompt. t 总是使用popup-menu
          popup-imenu-hide-rescan t     ;hide
          popup-imenu-force-position nil
          )
    (global-set-key (kbd "C-e") 'popup-imenu)

    ;; Close the popup with same key
    (define-key popup-isearch-keymap (kbd "C-e") 'popup-isearch-cancel)
    ))

;;; origami
;; (defun dingyi/init-origami ()
  ;; (use-package origami))
