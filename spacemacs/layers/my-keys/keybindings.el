
(spacemacs/set-leader-keys
  ;; I don't want close emacs daemon by SPC q q
  "qq" 'my/spacemacs-maybe-kill-emacs)

(evil-leader/set-key "qq" 'my/spacemacs-maybe-kill-emacs)
