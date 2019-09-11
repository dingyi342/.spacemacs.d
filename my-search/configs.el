    ;; (comma-def
    ;;   "s" (general-key-dispatch 'my/search
    ;;         :timeout 0.2
    ;;         "d" 'spacemacs/helm-dir-do-ag
    ;;         "s" 'helm-swoop
    ;;         "j" 'org-db-open-heading
    ;;         "p" 'spacemacs/helm-project-smart-do-search
    ;;         "o" (lambda ()
    ;;               (interactive)
    ;;               (let ((default-directory my-org-directory))
    ;;                 (spacemacs/helm-files-do-ag default-directory))
    ;;               )
    ;;         "e" (lambda ()
    ;;               (interactive)
    ;;               (let ((default-directory "~/.txnix/spacemacs/"))
    ;;                 (spacemacs/helm-files-do-ag default-directory))
    ;;               )
    ;;         "x" (lambda ()
    ;;               (interactive)
    ;;               (let ((default-directory "~/.txnix/"))
    ;;                 (spacemacs/helm-files-do-ag default-directory))
    ;;               )
    ;;         )
    ;;   )
    ;; (comma-def
    ;;   "f" (general-key-dispatch
    ;;           'my/deft-find-file
    ;;         :timeout 0.2
    ;;         "x" (lambda ()
    ;;               (interactive)
    ;;               (let ((default-directory "~/.txnix"))
    ;;                 (helm-projectile-find-file)
    ;;                 ;; (helm-projectile-find-file-dwim)
    ;;                 ))
    ;;         ))

    ;; (comma-def
    ;;   "j" (general-key-dispatch
    ;;           'org-db-open-heading
    ;;         ;; 'helm-org-agenda-files-headings
    ;;         ;; 'counsel-org-agenda-headlines
    ;;         :timeout 0.3
    ;;         "b" 'lazy-helm/spacemacs/helm-jump-in-buffer
    ;;         ;; "j" 'oi-jump
    ;;         "j" (lambda ()
    ;;               (interactive)
    ;;               (if (eq major-mode 'org-mode)
    ;;                   (spacemacs/helm-jump-in-buffer)
    ;;                 (oi-jump)
    ;;                 ))
    ;;         "n" 'helm-navi
    ;;         "m" 'helm-imenu
    ;;         "l" 'org-db-open-link-in-file
    ;;         "f" 'org-db-open-file
    ;;         "r" 'org-db-open-recent-file
    ;;         )
    ;;   )

