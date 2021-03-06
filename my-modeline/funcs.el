(defun dingyi/awesome-tray-enable ()
  (interactive)
  ;; Add update timer.
  (setq awesome-tray-timer
        (run-with-timer 0 0.5 'awesome-tray-show-info))
  (add-hook 'focus-in-hook 'awesome-tray-show-info)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (with-current-buffer (current-buffer)
                (if awesome-tray-active-p
		    (hide-mode-line-mode)
                    ;;  (spacemacs/toggle-mode-line-off)
		    )
                )
              ))
                                        ; Notify user.
  (setq awesome-tray-active-p t)
  (message "Enable awesome tray.")
  )

(defun dingyi/awesome-tray-disable ()
  (interactive)
  ;; show mode line
  ;; Cancel timer.
  (when (timerp awesome-tray-timer)
    (cancel-timer awesome-tray-timer))
  (remove-hook 'focus-in-hook 'awesome-tray-show-info)
  (remove-hook 'window-configuration-change-hook
               (lambda ()
                 (with-current-buffer (current-buffer)
                   (if awesome-tray-active-p
                       (spacemacs/toggle-mode-line-off))
                   )
                 ))

  ;; Update mode-line.
  (force-mode-line-update)
  (redraw-display)
  (with-current-buffer " *Minibuf-0*"
    (erase-buffer))
  ;; Notify user.
  (setq awesome-tray-active-p nil)
  (message "Disable awesome tray.")
  )
