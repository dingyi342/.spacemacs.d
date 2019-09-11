;;; packages.el --- my-emms layer packages file for Spacemacs.
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

;;; Code:

(defconst my-emms-packages
  '(
    ;; We need this recipe because MELPA version doesn't download the taglib metadata reader
    ;; emms 没有 taglib
    (emms :location (recipe
                     :fetcher github
                     :repo "emacsmirror/emms"
                     :files ("lisp/*.el"
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path" "Makefile")
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path/src" "src/*")
                             )))
    ;; emms-state
    helm-emms
    bongo
    ;; emms-bilibili
    ;emms-mark-ext
    emms-info-mediainfo
    ;emms-state
    ;emms-soundcloud
    ;emms-mode-line-cycle
    ;org-emms
    ;; emms-player-simple-mpv
    ;; emms-player-mpv-jp-radios
    ;; netease-music
    )
  )

;;; emms
(defun my-emms/init-emms ()
  (use-package emms
    ;; :defer (spacemacs/defer)
    :config
;;;; 基础设置
      (require 'emms)
      (require 'emms-setup)
      (emms-all)
      (emms-mode-line 0)
      (emms-playing-time 1)
      (emms-default-players)
      (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)

      (require 'emms-info)
      (setq emms-info-auto-update t)
      (setq emms-info-asynchronously t)

      (require 'emms-info-libtag)
      (add-to-list 'emms-info-functions 'emms-info-libtag)
      (setq emms-info-functions '(emms-info-libtag))

      ;; (require 'emms-info-mp3info)
      ;; (add-to-list 'emms-info-functions 'emms-info-mp3info)
      ;; ;; (setq emms-info-functions '(emms-info-mp3info))
      ;; (require 'emms-info-ogginfo)
      ;; (add-to-list 'emms-info-functions 'emms-info-ogginfo)

      ;; ;; (require 'emms-info-cueinfo)
      ;; (add-to-list 'emms-info-functions 'emms-info-cueinfo)

;;;; 设置emms 音频文件目录
      ;; ;; (setq emms-directory (concat spacemacs-cache-directory "emms"))

      ;; (setq my-emms-directory "~/Music/")
      (setq my-emms-directory "/mnt/e/musics")
      ;; (setq emms-source-file-default-directory my-emms-directory)
      ;; (emms-add-directory-tree my-emms-directory)

;;;; stumpwm/emms 调用
      (defun stumpwm/emms ()
        (let* (
               (my-emms-directory "/mnt/e/musics/")
               (emms-player-mpd-music-directory my-emms-directory)
               (emms-source-file-default-directory my-emms-directory))
          (if (get-process "emms-player-simple-process")
              (emms)
            (progn
              (emms-add-directory-tree my-emms-directory)
              ;; (emms-play-directory my-emms-directory)
              (emms)
              )
            )
          )
        )
;;;; stumpwm/emms-video 调用
      (defun stumpwm/emms-video ()
        (interactive)
        (let* (
               (my-emms-directory "/mnt/e/11/")
               (emms-player-mpd-music-directory my-emms-directory)
               (emms-source-file-default-directory my-emms-directory))
          (if (get-process "emms-player-simple-process")
              (emms)
            (progn
              (emms-add-directory-tree my-emms-directory)
              ;; (emms-play-directory my-emms-directory)
              (emms)
              )
            )
          )
        )

;;;; evil 快捷键
      (evil-collection-init 'emms)
      (general-def emms-playlist-mode-map
        ;; "m" 'emms-mark-track
        "m" 'emms-mark-forward
        "u" 'emms-mark-unmark-forward
        )

    )
  )

;; (defun my-emms/init-emms-state ()
;;   (use-package emms-state
;;     ;; for some reason if this is deferred you can't bring up the smart browser.
;;     :config
;;     ;; (emms-state-mode )
;;     ))

;;; helm-emms
(defun my-emms/init-helm-emms ()
  (use-package helm-emms
    :defer (spacemacs/defer)
    )
  )

;;; bongo 另一个emms
(defun my-emms/init-bongo ()
  (use-package bongo
    :defer (spacemacs/defer)
    :config
    (setq
     bongo-default-directory "/mnt/e/musics"
     bongo-backends '(mplayer mpg123 vlc  ogg123 speexdec timidity mikmod afplay)
     bongo-enabled-backends '(mplayer)
     )
    )
  )

;;; 没有用
(defun my-emms/init-emms-bilibili ()
  (use-package emms-bilibili
    :config
    (setq emms-bilibili-mid 2020166)
    )
  )

(defun my-emms/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo
    :defer (spacemacs/defer)
    :config
    ;; (add-to-list 'emms-info-functions #'emms-info-mediainfo)
    )
  )

(defun my-emms/init-emms-player-simple-mpv ()
  (use-package emms-player-simple-mpv
    :config
    (require 'emms-player-simple-mpv-control-functions)

    ;; An example of setting like emms-player-mplayer.el
    ;; `emms-player-my-mpv' is defined in this case.
    (define-emms-simple-player-mpv my-mpv '(file url streamlist playlist)
      (concat "\\`\\(http[s]?\\|mms\\)://\\|"
              (apply #'emms-player-simple-regexp
                     "aac" "pls" "m3u"
                     emms-player-base-format-list))
      "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

    (emms-player-simple-mpv-add-to-converters
     'emms-player-my-mpv "." '(playlist)
     (lambda (track-name) (format "--playlist=%s" track-name)))

    (add-to-list 'emms-player-list 'emms-player-my-mpv t)

    (dolist (map (list emms-playlist-mode-map
                       emms-stream-mode-map))
      (define-key map (kbd "m") 'emms-player-simple-mpv-mute)
      (define-key map (kbd "[") 'emms-player-simple-mpv-speed-decrease)
      (define-key map (kbd "]") 'emms-player-simple-mpv-speed-increase)
      (define-key map (kbd "{") 'emms-player-simple-mpv-speed-halve)
      (define-key map (kbd "}") 'emms-player-simple-mpv-speed-double)
      (define-key map (kbd "<backspace>") 'emms-player-simple-mpv-speed-normal)
      (define-key map (kbd "T") 'emms-player-simple-mpv-ontop)
      (define-key map (kbd "F") 'emms-player-simple-mpv-fullscreen)
      (define-key map (kbd "9") 'emms-volume-lower)
      (define-key map (kbd "0") 'emms-volume-raise))

    (let ((map emms-playlist-mode-map))
      (define-key map (kbd ",") 'emms-player-simple-mpv-playlist-prev)
      (define-key map (kbd ".") 'emms-player-simple-mpv-playlist-next))

    ;; Playing YouTube playlist in reverse order.
    ;; `emms-player-my-mpv-ytpl-reverse' will be defined in this case.
    (define-emms-simple-player-mpv my-mpv-ytpl-reverse '(url)
      "\\`http[s]://www\\.youtube\\.com/playlist\\?list="
      "mpv" "--no-terminal" "--force-window=no" "--audio-display=no"
      "--ytdl" "--ytdl-raw-options=playlist-reverse=")

    (add-to-list 'emms-player-list 'emms-player-my-mpv-ytpl-reverse t)
    ))

(defun my-emms/init-emms-player-mpv-jp-radios ()
  (use-package emms-player-mpv-jp-radios
    :config
    (emms-player-mpv-jp-radios-add-all)
    ))

;;; packages.el ends here

