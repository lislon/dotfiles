(setq my-keys-packages
      '(evil
        evil-unimpaired
        ))

(defun my-keys/post-init-evil ()
  (my/define-key evil-normal-state-map
    "+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
    "_" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt))

(defun my-keys/post-init-evil-unimpaired ()
  (my/define-key evil-normal-state-map
    "[s" 'my/spaces-before
    "]s" 'my/spaces-after))

(defun my-keys/post-init-helm ()
  ;; (spacemacs/set-leader-keys
  ;;   "ot" 'helm-top)
  )
