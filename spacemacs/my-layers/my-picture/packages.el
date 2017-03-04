(setq my-picture-packages '((picture :location built-in)))

(defun my-picture/init-picture ()
  (use-package picture
    :defer t
    :config

    (evil-define-key 'normal picture-mode-map
      "l" 'picture-motion
      "h" 'picture-motion-reverse
      "j" 'picture-move-down
      "k" 'picture-move-up
      "V" 'evil-visual-block
      "v" 'evil-visual-char
      "R" 'my/picture-wrap-with-rectangle
      "s" 'my/picture-select-rectangle
      "L" 'my/picture-move-right
      )
    (evil-define-key 'visual picture-mode-map
      "l" 'picture-motion
      "h" 'picture-motion-reverse
      "j" 'picture-move-down
      "k" 'picture-move-up
      "V" 'evil-visual-block
      "v" 'evil-visual-char
      )))
