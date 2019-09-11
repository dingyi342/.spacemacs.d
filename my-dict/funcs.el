
(defun goldendict-popup (region-beginning region-end)
  "query goldendict popup thing,就是通过读取选中的内容，然后拼接成一个命令行命令，传给goldendict"
  (interactive "r")
  (if (use-region-p)
      (start-process-shell-command
       "goldendict-popup" nil
       (concat "goldendict " (buffer-substring region-beginning region-end)))
    (interactive))
  (message "region not active"))

