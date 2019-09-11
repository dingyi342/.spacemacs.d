;;; packages.el --- my-lsp layer packages file for Spacemacs.
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

(defconst my-lsp-packages
  '(
    lsp-mode
    )
  )

(defun my-lsp/post-init-lsp-mode ()
  (use-package lsp-mode
    :config
    (setq centaur-lsp 'lsp-mode)
    (when centaur-lsp
      ;; Enable LSP in org babel
      ;; https://github.com/emacs-lsp/lsp-mode/issues/377
      (cl-defmacro lsp-org-babel-enbale (lang)
        "Support LANG in org source code block."
        (cl-check-type lang stringp)
        (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
               (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
          `(progn
             (defun ,intern-pre (info)
               (let ((filename (or (->> info caddr (alist-get :file))
                                   buffer-file-name)))
                 (setq buffer-file-name filename)
                 (pcase centaur-lsp
                   ('eglot
                    (and (fboundp 'eglot) (eglot)))
                   ('lsp-mode
                    (and (fboundp 'lsp)
                         ;; `lsp-auto-guess-root' MUST be non-nil.
                         (setq lsp-buffer-uri (lsp--path-to-uri filename))
                         (lsp-deferred))))))
             (put ',intern-pre 'function-documentation
                  (format "Enable `%s' in the buffer of org source block (%s)."
                          centaur-lsp (upcase ,lang)))

             (if (fboundp ',edit-pre)
                 (advice-add ',edit-pre :after ',intern-pre)
               (progn
                 (defun ,edit-pre (info)
                   (,intern-pre info))
                 (put ',edit-pre 'function-documentation
                      (format "Prepare local buffer environment for org source block (%s)."
                              (upcase ,lang))))))))

      (defvar org-babel-lang-list
        '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
      (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
      (dolist (lang org-babel-lang-list)
        (eval `(lsp-org-babel-enbale ,lang))))

    )
  )

;;; packages.el ends here
