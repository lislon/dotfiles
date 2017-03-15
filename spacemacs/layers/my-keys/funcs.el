(defun my/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (kbd (pop bindings)) (pop bindings))))

(defun my/spaces-before (n)
  (interactive "p")
  (dotimes (c n nil)
    (insert " ")))

(defun my/spaces-after (n)
  (interactive "p")
  (forward-char)
  (dotimes (c n nil)
    (insert " "))
  (backward-char (1+ n)))


(defun my/spacemacs-maybe-kill-emacs ()
  "If emacs server is running, kills frame instead of server"
  (interactive)
  ;; Check local buffer variable if we editing files from console.
  (if server-buffer-clients
      ;; if yes, just kill buffer it to return to console
      (kill-buffer)
    ;; Otherwise kill frame
    (if (server-running-p)
        (spacemacs/frame-killer)
      (spacemacs/kill-emacs))))
