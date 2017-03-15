(when (configuration-layer/package-usedp 'my-google-transte)
  (defun my/google-translate-repl ()
    (interactive)
    (require 'google-translate-default-ui)
    (let ((buffer (get-buffer-create "Google Translate REPL")))
      (switch-to-buffer buffer)
      (google-translate-interactive-mode)
      (evil-insert-state)
      (goto-char (buffer-end 1))
      ))

  ;; Google translate interactive mode
  (define-derived-mode google-translate-interactive-mode
    text-mode "Google Translate"
    (define-key google-translate-interactive-mode-map (kbd "RET") 'my/translate-word-and-next-line)
    (evil-define-key 'normal google-translate-interactive-mode-map (kbd "RET") 'my/translate-word-and-next-line)
    (add-hook 'google-translate-interactive-mode-hook
              (lambda ()
                (add-hook 'kill-buffer-hook
                          'my/google-translate-append-to-dictionary t t)))
    )

  (defun my/google-translate-append-to-dictionary ()
    "Appends words from interactive buffer to dictionary"
    (when my/english-dictionary-file
      (append-to-file (point-min) (point-max) my/english-dictionary-file)))


  (defun my/translate-word-and-next-line ()
    "Shows translation of current line in help buffer and inserts
new line after it"
    (interactive)
    (save-selected-window
      (move-beginning-of-line nil)
      (set-mark-command nil)
      (move-end-of-line nil)
      (google-translate-at-point))
    (if (eq (point) (point-max))
        (newline-and-indent)
      (end-of-line 2))))
