;https://mollermara.com/blog/Fast-refiling-in-org-mode-with-hydras/
(defun my/refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))


(defhydra josh/org-refile-hydra (:foreign-keys run) 
  "Refile"
  ("g" (my/refile "shopping.org" "Grocery store") "Refile to Grocery store")
  ("o" (my/refile "shopping.org" "Office supplies") "Refile to Office supplies")
  ("e" (my/refile "tasks.org" "Email tasks") "Email tasks")
  ("r" (my/refile "tasks.org" "Research tasks") "Research tasks")
  ("j" org-refile-goto-last-stored "Jump to last refile")
  ("q" nil "cancel"))
;; Or whatever you want your keybinding to be
(global-set-key (kbd "<f9> r") 'josh/org-refile-hydra/body)


(defhydra josh/org-refile-hydra (:foreign-keys run) 
  "Refile"
  ("g" (my/refile "shopping.org" "Grocery store") "Grocery store")
  ("o" (my/refile "shopping.org" "Office supplies") "Office supplies")
  ("a" (my/refile "shopping.org" "Buy on Amazon") "Buy on Amazon")
  ("e" (my/refile "tasks.org" "Email tasks") "Email tasks")
  ("r" (my/refile "tasks.org" "Research tasks") "Research tasks")
  ("c" (my/refile "tasks.org" "Calendar") "Calendar")
  ("p" (my/refile "tasks.org" "Projects") "Projects")
  ("s" (my/refile "someday-maybe.org" "Someday/Maybe") "Someday/Maybe")
  ("b" (my/refile "media.org" "Books to read") "Books to read")
  ("m" (my/refile "media.org" "Movies to watch") "Movies to watch")
  ("E" (my/refile "ideas.org" "Emacs ideas") "Emacs ideas")
  ("J" (my/refile "ideas.org" "Jokes") "Jokes")
  ("j" org-refile-goto-last-stored "Jump to last refile")
  ("q" nil "cancel"))

(defhydra josh/org-refile-hydra-file-a
  (:color blue :after-exit (josh/org-refile-hydra/body))
  "File A"
  ("1" (my/refile "file-a.org" "Headline 1") "Headline 1")
  ("2" (my/refile "file-a.org" "Headline 2") "Headline 2")
  ("q" nil "cancel"))
(defhydra josh/org-refile-hydra-file-b
  (:color blue :after-exit (josh/org-refile-hydra/body))
  "File B"
  ("1" (my/refile "file-b.org" "One") "One")
  ("2" (my/refile "file-b.org" "Two") "Two")
  ("q" nil "cancel"))
(defhydra josh/org-refile-hydra-file-c
  (:color blue :after-exit (josh/org-refile-hydra/body))
  "File C"
  ("1" (my/refile "file-c.org" "1") "1")
  ("2" (my/refile "file-c.org" "2") "2")
  ("q" nil "cancel"))
(defhydra josh/org-refile-hydra (:foreign-keys run) 
  "Refile"
  ("a" josh/org-refile-hydra-file-a/body "File A" :exit t)
  ("b" josh/org-refile-hydra-file-b/body "File B" :exit t)
  ("c" josh/org-refile-hydra-file-c/body "File C" :exit t)
  ("q" nil "cancel"))


