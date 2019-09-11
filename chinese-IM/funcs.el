;; (when (configuration-layer/package-used-p 'flypy)
(defun activate-default-input-method ()
  (interactive)
  (activate-input-method default-input-method))
;; )

(defun deactivate-current-input-method ()
  (interactive)
  (if current-input-method
      (progn
        (deactivate-input-method)
        (setq current-input-method nil)
        ;; (setq current-input-method "")
        (setq evil-input-method nil)
        )
    ))
;; @https://github.com/tumashu/pyim-wbdict/issues/3

(defun pyim-page-select-second-word ()
  (interactive)
  (pyim-page-select-word-by-number 2))

(defun pyim-page-select-third-word ()
  (interactive)
  (pyim-page-select-word-by-number 3))


(defun toggle-pyim-and-flypy ()
  "在pyim和fypyl中进行切换"
  (interactive)
  (if (eq default-input-method 'pyim)
      (set-input-method 'chinese-flypy)
    (set-input-method 'pyim)))
