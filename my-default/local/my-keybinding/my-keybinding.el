;;; my-kebinding.el --- my default keybinding.       -*- lexical-binding: t; -*-
;; Copyright (C) 2019
;; Author:  <dingyi@dingyi>
;; Keywords:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun my-keybinding-init ()
  (interactive)
;;; comma-def starts
;;;; a
  (comma-def
    "a" 'org-agenda
;;;; b
    ;; "b" 'helm-mini
    "b" (general-key-dispatch 'helm-mini
          :timeout 0.3
          "m" 'emms)
;;;; c
    ;; "c" 'org-capture
    "c" (general-key-dispatch
            'emacs/find-today-file
          ;; 'org-capture
          :timeout 0.2
          "c" 'my-org-deepin-screenshot
          "s" 'org-download-screenshot
          "d" 'my-deepin-screenshot
          )
;;;; d
    ;; "d" 'save-and-bury-buffer
    "d" 'bury-buffer
;;;; e
    "e" 'ediff-current-file
;;;; f
    "f" (general-key-dispatch
            (lambda ()
              (interactive)
              (let ((completing-read-function 'ivy-completing-read))
                (call-interactively 'my/deft-find-file)
                ))
          :timeout 0.3
          "x" (lambda ()
                (interactive)
                (let ((default-directory "~/.txnix"))
                  (helm-projectile-find-file)
                  ;; (helm-projectile-find-file-dwim)
                  ))
          "e" (lambda ()
                (interactive)
                (let ((default-directory "~/.txnix/spacemacs/"))
                  (helm-projectile-find-file)
                  ;; (helm-projectile-find-file-dwim)
                  ))
          )
;;;; g
    "g" (general-key-dispatch
            'magit-status
          :timeout 0.3
          "c" 'magit-clone
          "m" 'magit-dispatch-popup
          "t" 'spacemacs/time-machine-transient-state/body
          "x" (lambda ()
                (interactive)
                (magit-status-setup-buffer "~/.txnix"))
          "e" (lambda ()
                (interactive)
                (magit-status-setup-buffer "~/.txnix/+submodules/spacemacs/"))
          "o" (lambda ()
                (interactive)
                (magit-status-setup-buffer my-org-directory))
          )
;;;; h
    "h" (general-key-dispatch 'helm-info-at-point
          :timeout 0.3
          "f" 'describe-function
          "k" 'describe-bindings
          "c" 'describe-char
          "F" 'describe-face
          "p" 'describe-package
          "v" 'describe-variable)
;;;; i
    "i" (general-key-dispatch 'insert-char
          :timeout 0.3
          "t" 'spacemacs/timestamp-transient-state/body
          "d" 'help/insert-datestamp
          "f" 'cnfonts-insert-fontname
          "y" 'yas-insert-snippet
          )
;;;; j
    "j" (general-key-dispatch
            ;; 'org-db-open-heading
            'ivy/org-db-open-heading
          ;; 'helm-org-agenda-files-headings
          ;; 'counsel-org-agenda-headlines
          :timeout 0.3
          "b" 'lazy-helm/spacemacs/helm-jump-in-buffer
          "f" 'org-db-open-file
          "j" (lambda ()
                (interactive)
                (if (eq major-mode 'org-mode)
                    (spacemacs/helm-jump-in-buffer)
                  (oi-jump)
                  ))
          "l" 'org-db-open-link-in-file
          "m" 'helm-imenu
          "n" 'helm-navi
          "r" 'org-db-open-recent-file
          )
;;;; k
    "k" 'spacemacs/kill-this-buffer
;;;; q
    "q" 'kill-buffer-and-window
;;;; r
    "r" (general-key-dispatch
            ;; 'helm-org-rifle 'helm-recentf
            :timeout 0.3
            "r" 'helm-org-rifle
            "b" 'helm-org-rifle-current-buffer
            "d" 'helm-org-rifle-directories
            "o" 'helm-org-rifle-org-directory
            "a" 'helm-org-rifle-agenda-files
            )
;;;; s
    "s" (general-key-dispatch 'my/search
          :timeout 0.3
          "b" 'helm-chrome-bookmarks
          "g" 'engine/search-google
          "t" 'goldendict-dwim
          "m" 'fuo-search
          "d" 'spacemacs/helm-dir-do-ag
          "f" 'helm-ag-search-flypy
          "s" 'helm-swoop
          "j" 'org-db-open-heading
          "p" 'spacemacs/helm-project-smart-do-search
          "o" (lambda ()
                (interactive)
                (let ((default-directory my-org-directory))
                  (spacemacs/helm-files-do-ag default-directory))
                )
          "e" (lambda ()
                (interactive)
                (let ((default-directory "~/.txnix/spacemacs/"))
                  (spacemacs/helm-files-do-ag default-directory))
                )
          "x" (lambda ()
                (interactive)
                (let ((default-directory "~/.txnix/"))
                  (spacemacs/helm-files-do-ag default-directory))
                )
          "r" (lambda ()
                (interactive)
                (helm-org-rifle-directories '("~/.txnix/.org_notes/notes/") t)
                )
          )
;;;; S
    "S" 'org-download-screenshot
    ;; "S" 'my-deepin-screenshot
;;;; t
    "t" 'counsel-M-x
    ;; (lookup-key spacemacs-default-map (kbd "f"))
;;;; spc
    "SPC" (copy-keymap spacemacs-default-map)
;;;; comma-def ends
    )
;;; evilified motion 补充
  (general-def
    :states '(evilified motion)
    :keymaps 'override
    :prefix ","
    "k" 'spacemacs/kill-this-buffer
    "q" 'kill-buffer-and-window
    "d" 'bury-buffer
    )
;;; M/Alt 修饰符
;;;; M-1
  (emap "M-1" (lambda ()
                (interactive)
                (find-file "~/.txnix/.org_notes/notes/emacs.org")
                ))
;;;; M-2
  (emap "M-2" (lambda ()
                (interactive)
                (find-file "~/.txnix/.org_notes/notes/linux.org")
                ))

;;;; M-3
  (emap "M-3" (lambda ()
                (interactive)
                (find-file "~/.txnix/.org_notes/notes/dingyi.org")
                ))

;;;; M-4
  (emap "M-4" (lambda ()
                (interactive)
                (find-file "~/.txnix/.org_notes/notes/emacs.org")
                ))

;;;; M-5
  (emap "M-5" (lambda ()
                (interactive)
                (find-file "~/.txnix/.org_notes/notes/emacs.org")
                ))

;;;; M-6
;;;; M-7
;;;; M-8
;;;; M-9
;;;; M-0
;;;; M-a
;;;; M-b
;;;; M-c
;;;; M-d
  (nmap :keymaps 'override "M-d" 'bury-buffer)
;;;; M-e
;;;; M-f
  (nmap outline-mode-map "M-f" 'spacemacs/counsel-jump-in-buffer)
  (nmap outshine-mode-map "M-f" 'spacemacs/counsel-jump-in-buffer)
  ;; (nmap outshine-mode-map "M-f" 'outshine-imenu)
;;;; M-g
;;;; M-h
  (nmap :keymaps 'override "M-h" 'iflipb-previous-buffer)
  ;; (imap :keymaps 'override "M-h" 'backward-kill-word)
  (imap org-mode-map "M-h" 'org-metaleft)
  (emap org-mode-map "M-h" 'org-metaleft)
;;;; M-i
;;;; M-j
;; (nmap :keymaps 'override "M-j" 'ivy-iflipb)
(nmap :keymaps 'override "M-j" 'awesome-fast-switch/body)
;;;; M-k
  (nmap :keymaps 'override "M-k" 'spacemacs/kill-this-buffer)
;;;; M-l
  (nmap :keymaps 'override "M-l" 'iflipb-next-buffer)
  (emap org-mode-map "M-l" 'org-metaright)
;;;; M-m
;;;; M-n
;;;; M-o
  (nmap :keymaps 'override "M-o" 'other-windows-2)
  (nmap :keymaps 'override "M-e" 'evil-execute-in-emacs-state)
  (nmap :keymaps 'override "M-i" 'evil-execute-in-insert-state)

  ;; (imap :keymaps 'override "M-i" 'evil-execute-in-emacs-state)
  (imap :keymaps 'override "M-o" 'evil-execute-in-normal-state)
  (imap :keymaps 'override "M-e" 'evil-execute-in-emacs-state)

  ;; (emap :keymaps 'override "M-e" 'evil-execute-in-insert-state)
  (emap :keymaps 'override "M-o" 'evil-execute-in-normal-state)
  (emap :keymaps 'override "M-i" 'evil-execute-in-insert-state)
;;;; M-p
;;;; M-q
;;;; M-r
(nmap :keymaps 'override "M-r" 'counsel-abm-recent)
;;;; M-s
  (nmap :keymaps 'override "M-s" 'my/save-and-magit-unstaged-current-buffer)
  (imap :keymaps 'override "M-s" 'my/save-and-magit-unstaged-current-buffer)
  ;; (nmap "M-s" 'evil-write-all)
;;;; M-t
;;;; M-u
  (nmap :keymaps 'override "M-u" 'winner-undo)
  (nmap :keymaps 'override "M-U" 'winner-redo)
;;;; M-v
;; (nmap "M-v" 'counsel-abm-visited)
(nmap :keymaps 'override "M-v" 'vterm-toggle)
;;;; M-w
;;;; M-x
  (nmap "M-x" 'counsel-M-x)
  (imap "M-x" 'counsel-M-x)
  ;; (imap "M-x" 'helm-M-x)
  (emap "M-x" 'helm-M-x)
;;;; M-y
;;;; M-z

;;;; M-TAB
(nmap :keymaps 'override "M-TAB" 'spacemacs/alternate-buffer)
(imap :keymaps 'override "M-TAB" 'spacemacs/alternate-buffer)
(emap :keymaps 'override "M-TAB" 'spacemacs/alternate-buffer)
;;; Ctrl 修饰符
;;;; C-a
  ;; (nmap "C-a" 'mwim-beginning-of-code-or-line)
  ;; (imap "C-a" 'evil-paste-last-insertion)
;;;; C-b
  ;; (nmap "C-b" 'evil-scroll-page-up)
;;;; C-c

;;;; C-d
  ;; (nmap "C-d" 'evil-scroll-down)
;;;; C-e
  ;; (nmap "C-e" 'evil-scroll-line-down)
;;;; C-f
  ;; (nmap "C-f" 'evil-scroll-page-down)
;;;; C-g
;;;; C-h
;; (imap "C-h" 'delete-backward-char)
(nmap "C-h f" 'describe-function)
(nmap "C-h v" 'describe-variable)

;;;; C-i
  ;; (nmap "C-i" 'evil-jump-forward)
;;;; C-j
  ;; (nmap "C-j" 'electric-newline-and-maybe-indent)
;;;; C-k
  ;; (nmap "C-k" 'kill-line)
;;;; C-l
  ;; (nmap "C-l" 'recenter-top-bottom)
;;;; C-m
  ;; (nmap "C-m" 'evil-ret)
;;;; C-n
  ;; (nmap "C-n" 'evil-mc-make-and-goto-next-cursor)
  ;; (nmap "C-n" 'evil-paste-pop-next)
;;;; C-o
  ;; (nmap "C-o" 'evil-jump-backward)
;;;; C-p
  ;; (nmap "C-p" 'evil-paste-pop)
;;;; C-q
  ;; (nmap "C-q" 'quoted-insert)
;;;; C-r
  ;; (nmap "C-r" 'undo-tree-redo)
;;;; C-s
  (nmap "C-s" 'swiper)
  ;; (nmap "C-s" 'isearch-forward)
;;;; C-t
  ;; (nmap "C-t" 'evil-mc-skip-and-goto-next-cursor)
  ;; (nmap "C-t" 'pop-tag-mark)
;;;; C-u
  ;; (nmap "C-u" 'evil-scroll-up)
;;;; C-v
  ;; (nmap "C-v" 'evil-visual-block)
;;;; C-w

;;;; C-x
;;;; C-y
  ;; (nmap "C-y" 'evil-scroll-line-up)
;;;; C-z
  ;; (nmap "C-z" 'evil-emacs-state)
;;; Fn 功能键
;;;; F1
;;;; F2
;;;; F3
;;;; F4
;;;; F5
;;;; F6
;;;; F7
;;;; F8
;;;; F9
;;;; F10
;;;; F11
;;;; F12
;;;; F13
;;;; F14
;;;; F15
;;;; F16
;;;; F17
;;;; F18
;;;; F19
;;;; F20
;;;; F21
;;;; F22
;;;; F23
;;;; F24
;;;; F25
;;; semicolon-def
  (semicolon-def "s" 'save-buffer)
;;; \ -Def
  ;; 这个会提示 \\ 不是 prefix, 后面的都不会加载
  ;; (general-unbind 'normal "\\")
  ;; (general-create-definer \\-def
  ;;   :states '(normal evilified motion)
  ;;   ;; :keymaps 'override
  ;;   :prefix "\\")
  ;; (\\-def "s" 'save-buffer)

;;; θφρσβυηνμψϊπκ]
  ;; (general-def
  ;;   :states '(insert emacs)
  ;;   "θ" "\""
  ;;   "ρ" "["
  ;;   "σ" "{"
  ;;   "φ" "("
  ;;   )

  (general-def
    :states '(normal visual)
    "ρ" (general-simulate-key "[")
    "Ρ" (general-simulate-key "]")
    "ψ" (general-simulate-key "]")

    "σ" (general-simulate-key "{")
    "Σ" (general-simulate-key "}")
    "ν" (general-simulate-key "}")

    "φ" (general-simulate-key "(")
    "Φ" (general-simulate-key ")")
    "η" (general-simulate-key ")")

    "ϊ" (general-simulate-key "|")
    )
  (general-def
    :keymaps 'dired-mode-map
    :states '(normal)
    "η" (general-simulate-key "^")
    )

  ;; (general-def
  ;;   :states 'normal
  ;;   "ρ"  "["
  ;;   "Ρ"  "]"
  ;;   "ψ"  "]"

  ;;   "σ"  "{"
  ;;   "Σ"  "}"
  ;;   "ν"  "}"

  ;;   "φ"  "("
  ;;   "Φ"  ")"
  ;;   "η"  ")"

  ;;   "ϊ"  "|"
  ;;   )

  ;; (general-def
  ;;   :states 'normal
  ;;   :keymaps 'dired-mode-map
  ;;   "φ" (general-simulate-key "(")
  ;;   )

  ;; (general-translate-key nil 'evil-normal-state-map
  ;;   "ρ"  "["
  ;;   "ρ"  "]"
  ;;   "ψ"  "]"

  ;;   "σ"  "{"
  ;;   "σ"  "}"
  ;;   "ν"  "}"

  ;;   "φ"  "("
  ;;   "φ"  ")"
  ;;   "η"  ")"

  ;;   "ϊ"  "|"
  ;; )

  (general-def
    :states '(insert)
    "θ" 'ins-quotes
    "ρ" 'ins-brackets
    "σ" 'ins-braces
    "φ" 'ins-parens
    )

  (general-def
    :states '(emacs)
    "θ" 'ins-quotes
    "ρ" 'ins-brackets
    "σ" 'ins-braces
    "φ" 'ins-parens
    )

;;; evil 按键重新设置
  (general-nmap
    "[ρ" "[["
    "[ψ" "[]"

    "[φ" "[("
    "[{" "[{"

    "]ψ" "]]"
    "]ρ" "]["
    "]η" "])"
    "]ν" "]}"

    )

;;;; a
;;;; b
;;;; c
;;;; d
;;;; e
;;;; f
  ;; (nvmap "f" 'evil-avy-goto-char-in-line)
  ;; (mmap "f" 'evil-avy-goto-char-in-line)
  ;; (nvmap "f" 'evil-avy-goto-char-timer)
  (general-def
    :states '(normal visual motion)
    "f" (general-key-dispatch 'evil-avy-goto-char-in-line
          :timeout 0.21
          ;; :inherit-keymap evil-window-map
          "b" 'avy-pop-mark
          "c" 'goto-last-change
          "f" 'evil-avy-goto-char-in-line
          "j" 'evil-avy-goto-char-timer
          "l" 'evil-avy-goto-line
          "g" 'evil-avy-goto-char-2
          "w" 'evil-avy-goto-word-or-subword-1
          "u" 'spacemacs/avy-goto-url
          "o" 'spacemacs/avy-open-url
          "a" 'spacemacs/push-mark-and-goto-beginning-of-line
          "k" 'spacemacs/evil-goto-next-line-and-indent
          "e" 'spacemacs/push-mark-and-goto-end-of-line
          ;; "l" 'spacemacs/push-mark-and-goto-end-of-line
          ))

;;;; g
(nmap "g:" 'my/magit-diff-buffer-file)
;;;; h
  (nmap "h" (general-key-dispatch 'evil-backward-char
              :timeout 0.20
              "h" 'evil-backward-char
              "a" 'evil-prev-buffer
              ))

  ;; (nmap "h" 'evil-backward-char)
;;;; i
;;;; j
  ;; (nmap "j" (general-key-dispatch 'evil-next-line
  ;;             :timeout 0.2
  ;;             "j" 'evil-next-line
  ;;             "a" 'evil-beginning-of-line
  ;;             "e" 'evil-end-of-line
  ;;             ))

  (nmap "j" 'evil-next-line)
;;;; k
  ;; (nmap "k" (general-key-dispatch 'evil-previous-line
  ;;             :timeout 0.2
  ;;             "k" 'evil-previous-line
  ;;             ))
  (nmap "k" 'evil-previous-line)

;;;; l
  (nmap "l" (general-key-dispatch 'evil-forward-char
              :timeout 0.20
              "l" 'evil-forward-char
              "f" 'isearch-forward
              ))

  ;; (nmap "l" 'evil-forward-char)
;;;; m
  ;; (nmap "m" (general-key-dispatch 'evil-set-marker
  ;;             :timeout 0.20
  ;;             :inherit-keymap  (symbol-value (intern (format "spacemacs-%s-map" major-mode)))
  ;;             ))

;;;; n
;;;; o
  (general-def
    :states '(normal visual motion)
    "o" (general-key-dispatch 'evil-open-below
          :timeout 0.21
          ;; :inherit-keymap evil-window-map
          "b" 'avy-pop-mark
          "c" 'goto-last-change
          "f" 'evil-avy-goto-char-timer
          "j" 'evil-avy-goto-char-timer
          "l" 'evil-avy-goto-line
          "g" 'evil-avy-goto-char-2
          "w" 'evil-avy-goto-word-or-subword-1
          "u" 'spacemacs/avy-goto-url
          "o" 'spacemacs/avy-open-url
          "a" 'spacemacs/push-mark-and-goto-beginning-of-line
          "k" 'spacemacs/evil-goto-next-line-and-indent
          "e" 'spacemacs/push-mark-and-goto-end-of-line
          ;; "l" 'spacemacs/push-mark-and-goto-end-of-line
          ))

;;;; p
;;;; q
;;;; r
;;;; s
;;;; t
  (nvmap "t" 'evil-avy-goto-char-2)
  (mmap "t" 'evil-avy-goto-char-2)
  ;; (nvmap "t" 'evil-avy-goto-char-timer)
;;;; g
;;;; u
;;;; v
;;;; w
  ;; (nmap "w" 'evil-forward-word-begin)
  ;; (nmap "w" :inherit-keymap evil-window-map)
  ;; (mmap "w" :inherit-keymap evil-window-map)
  ;; (general-create-definer
  ;;   :keymaps 'normal
  ;;   "w" (general-key-dispatch 'evil-forward-word-begin
  ;;         :timeout 0.20
  ;;         :inherit-keymap evil-window-map
  ;;         ))
  (nmap "w" (general-key-dispatch 'evil-forward-word-begin
              :timeout 0.20
              :inherit-keymap evil-window-map
              ))
;;;; x
;;;; y
;;;; z

;;; evil-ex
;;;; a
;;;; b
;;;; c
;;;; d
;;;; e
;;;; f
;;;; g
;;;; h
;;;; i
;;;; j
;;;; k
;;;; l
;;;; m
;;;; n
;;;; o
;;;; p
;;;; q
;;;; r
;;;; s
;;;; t
;;;; u
;;;; v
;;;; w
;;;; x
;;;; y
;;;; z
;;; my-keybinding-init ends
  )
;;; my-kebinding.el ends here
(provide 'my-keybinding)
