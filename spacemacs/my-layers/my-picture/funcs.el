
(defun my/picture-select-rectangle ()
  "Selects a rectangle bounding cursor"
  (interactive)
  (let* (a b)
    (search-forward "-+")
    (setq a (point))
    (push-mark nil nil t)
    (search-backward "+-")
    (search-backward "+-")
    (setq b (point))
    ))

(defun my/picture-move-right ()
  "docstring"
  (interactive)
  (my/picture-select-rectangle)
  (call-interactively 'picture-clear-rectangle)
  ;; (call-interactively 'picture-motion)
  (picture-yank-rectangle))

(defun my/picture-wrap-with-rectangle ()
  "selects current word or selection and wraps it with
rectangle"
  (interactive)
  (my/exec-macro (kbd "viWVohkojl"))
  (call-interactively 'picture-draw-rectangle))

(defun my/exec-macro (macro &optional count)
  "Execute keyboard macros with single undo and recovering from errors"
  (condition-case err
      (evil-with-single-undo
        (execute-kbd-macro macro count))
    ;; enter Normal state if the macro fails
    (error
     (evil-normal-state)
     (evil-normalize-keymaps)
     (signal (car err) (cdr err)))))
