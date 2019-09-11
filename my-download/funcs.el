(defun stumpwm/aria2-download-url ()
  (interactive)
  (let ((url (read-from-minibuffer "url :"))
        (dir "/mnt/e/03videos/aria2/")
        (proxy "127.0.0.1:42647")
        (name (help/get-timestamp))
        )
    ;; (async-shell-command (format "proxychains aria2c --dir=%S --all-proxy=%S %S" dir proxy url))
    (async-shell-command (format "proxychains aria2c --dir=%s --out=%s %S" dir name url))
    )
  )


