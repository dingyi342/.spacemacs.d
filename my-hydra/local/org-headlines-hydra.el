(defhydra org-headlines-hydra (:color pink :hint nil )
  "
_n_ next _p_ pre _q_ nil
_k_ up _j_ down _h_ rigth _l_ left
_H_ ml _J_ md   _K_ mu    _L_ mr
_M-h_ sml  _M-j_ smd _M-k_ smu  _M-l_ smr
"
  ("n" org-next-visible-heading)
  ("p" org-previous-visible-heading)
  ;; ("j" org-next-visible-heading)
  ;; ("k" org-previous-visible-heading)

  ("h" org-up-element)
  ("j" org-forward-element)
  ("k" org-backward-element)
  ("l" org-down-element)

  ("H" org-metaleft)
  ("J" org-metadown)
  ("K" org-metaup)
  ("L" org-metaright)

  ("M-h" org-shiftmetaleft)
  ("M-j" org-shiftmetadown)
  ("M-k" org-shiftmetaup)
  ("M-l" org-shiftmetaright)


  ("q" nil)
  )




