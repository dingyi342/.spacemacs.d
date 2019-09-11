(defhydra helm-ff-hydra (:color pink :hint nil :timeout 5)
  "
_m_ music _v_ video
"
  ("m" (insert "~/Music/musics") :exit t)
  ("v" (insert "~/Videos/03videos") :exit t)
  ("q" nil)
  )

(general-de
  ;; helm-find-files-map
  ;; helm-generic-files-map
  ;; minibuffer-inactive-mode-map
  ;; minibuffer-local-filename-completion-map
  helm-map
  "M-p" 'helm-ff-hydra/body)


(spacemacs|define-transient-state helm-ff-hydra-2
  :title "helm-ff Transient State"
  :doc "
_m_ music  _v_ video
"
  :on-enter (helm-find-files default-directory)
  ;; :on-exit ()
  :bindings
  ("m" (insert "~/Music/musics") :exit t)
  ("v" (insert "~/Videos/03videos") :exit t)
  ("q" nil :exit t))



