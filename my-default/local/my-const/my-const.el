;;; my-const.el --- Define constants.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Define constants.
;;

;;; Code:

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defconst my-org-directory "~/.txnix/.org_notes/")
(defconst my-org-notes-directory (concat my-org-directory "notes/"))
(defconst my-org-diary-directory (concat my-org-directory "diary/"))
(defconst my-org-gtd-directory (concat my-org-directory "gtd/"))

(defconst my-today-file-name (concat (format-time-string "%Y-%m-%d") ".org"))

(defconst my-today-file-path (concat my-org-diary-directory my-today-file-name))

(provide 'my-const)

;;; my-const.el ends here
