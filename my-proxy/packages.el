;;; packages.el --- proxy layer packages file for Spacemacs.
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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `proxy-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `proxy/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `proxy/pre-init-PACKAGE' and/or
;;   `proxy/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-proxy-packages
  '(
    proxy-mode
    )
)

(defun my-proxy/init-proxy-mode ()
  (use-package proxy-mode
    :config
    ;; (setq
    ;;  ;; proxy-mode-url-proxy
    ;;  proxy-mode-http-proxy "http://localhost:42647"
    ;;  ;; proxy-mode-proxy-type
    ;;  proxy-mode-socks-proxy '("Default server" "127.0.0.1" 34335 5)
    ;;  ;; proxy-mode-rules-alist
    ;;  ;; proxy-mode-types
    ;;  ;; proxy-mode-hook
    ;;  )
    ;; (proxy-http-disable)
    ;; (setq socks-override-functions 1)
    ;; (setq socks-noproxy '("localhost"))
    ;; (require 'socks)
    ;; (setq url-gateway-method 'socks)
    ;; (setq socks-server '("Default server" "localhost" 34335 5))
    ;; (setq url-proxy-services
    ;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
    ;;         ("http" . "localhost:42647")
    ;;         ("https" . "localhost:42647")))
    ;; (setq url-proxy-services
    ;;       '(
    ;;         ("http"     . "http://127.0.0.1:42647")
    ;;         ("https"    . "http://127.0.0.1:42647")
	  ;;         ("ftp"      . "http://127.0.0.1:42647")
    ;;         ("no_proxy" . "^\\(localhost\\|10.*\\)")
    ;;         ))
    ))


;;; packages.el ends here
