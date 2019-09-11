(spacemacs|define-transient-state cnfonts
  :title "cnfonts profile"
  :doc "\n[_+_/_=_] increase fontsize [_-_] decrease fontsize [_0_] reset
[_e_] edit profile  [_E_] edit file without-ui
[_n_] next profile [_SPC_] next profile [_s_] switch profile 
[_i_] insert fontname [_I_] insert fonts configure [_q_] quit"
  :bindings
  ;; ("1" )
  ("e" cnfonts-edit-profile)
  ("n" cnfonts-next-profile)
  ("SPC" cnfonts-next-profile)
  ("E" cnfonts-edit-profile-without-ui)
  ("s" cnfonts-switch-profile)
  ("i" cnfonts-insert-fontname)
  ("I" cnfonts-insert-fonts-configure)
  ("0" cnfonts-reset-fontsize)
  ("+" cnfonts-increase-fontsize)
  ("=" cnfonts-increase-fontsize)
  ("-" cnfonts-decrease-fontsize)
  ("q" nil :exit t))
