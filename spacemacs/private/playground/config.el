(defun w32-browser (doc) (w32-shell-execute 1 doc))

(eval-after-load "dired" '(define-key dired-mode-map [f3] (lambda () (interactive) (w32-browser (dired-replace-in-string "/" "\\" (dired-get-filename))))))
