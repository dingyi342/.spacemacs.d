;;; packages.el --- timestamp layer packages file for Spacemacs.
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
;; added to `timestamp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `timestamp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `timestamp/pre-init-PACKAGE' and/or
;;   `timestamp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst timestamp-packages
  '(
    (timestamp :location local)
    ))

;;; packages.el ends here

;; (defun timestamp/post-init-hydra ()
;;   (comma-def :infix "i"
;;     "t" 'help/hydra/timestamp/body
;;     "T" 'spacemacs/timestamp-transient-state/body
;;     )

;;   (with-eval-after-load hydra
;;     (defhydra help/hydra/timestamp (:color blue :hint nil)
;;       "
;;         Timestamps: (_q_uit)
;;         Date: _I_SO, _U_S, US With _Y_ear and _D_ashes, US In _W_ords
;;         Date/Time: _N_o Colons or _w_ith
;;         Org-Mode: _R_ight Now or _c_hoose
;;         "
;;       ("q" nil)

;;       ("I" help/insert-datestamp)
;;       ("U" help/insert-datestamp-us)
;;       ("Y" help/insert-datestamp-us-full-year)
;;       ("D" help/insert-datestamp-us-full-year-and-dashes)
;;       ("W" help/insert-datestamp-us-words)

;;       ("N" help/insert-timestamp-no-colons)
;;       ("w" help/insert-timestamp)

;;       ("R" help/org-time-stamp-with-seconds-now)
;;       ("c" org-time-stamp))
;;     )

;;   )

(defun timestamp/init-timestamp ()
  (use-package timestamp
    :config
    (spacemacs|define-transient-state timestamp
      :title "Timestamp Transient State"
      :doc "
        Timestamps: (_q_uit)
        Date: _I_SO, _U_S, US With _Y_ear and _D_ashes, US In _W_ords
        Date/Time: _N_o Colons [_w_] insert/timestamp
        Org-Mode: [_r_] org-time-stamp-with-seconds [_c_] org-time-stamp"
      :hint nil
      :color blue
      :foreign-keys run
      :bindings
      ("q" nil)

      ("I" help/insert-datestamp)
      ("U" help/insert-datestamp-us)
      ("Y" help/insert-datestamp-us-full-year)
      ("D" help/insert-datestamp-us-full-year-and-dashes)
      ("W" help/insert-datestamp-us-words)

      ("N" help/insert-timestamp-no-colons)
      ("w" help/insert-timestamp)

      ("r" help/org-time-stamp-with-seconds-now)
      ("c" org-time-stamp))

    (spacemacs/set-leader-keys "it" 'spacemacs/timestamp-transient-state/body)
    )
  )
