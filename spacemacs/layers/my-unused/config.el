(evil-define-motion evil-search-forward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (progn                 ;MADE CHANGES HERE
    (evil-insert-state)
    (evil-rch-incrementally t evil-regexp-search)
    (evil-normal-state)
    ))

(evil-define-motion evil-search-backward ()
  (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
          (if (and (fboundp 'isearch-forward)
                   (documentation 'isearch-forward))
              (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                      (documentation 'isearch-forward)) ""))
  :jump t
  :type exclusive
  :repeat evil-repeat-search
  (progn                 ;MADE CHANGES HERE
    (evil-insert-state)
    (evil-search-incrementally nil evil-regexp-search)
    (evil-normal-state)
    ))
;; SPC aw for weather
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (evil-leader/set-key "aw" (lambda ()
                              (interactive)
                              (wttrin-query "St.Petersburg")))
  :config
  )

(use-package smsru
  :commands (smsru/send-sms)
  :defer t)

(evil-leader/set-key
    "aiq" 'erc-quit-server
    "aid" (defun bb/gitter-debug ()
            (interactive)
            (erc :server "localhost"
                 :port 6667
                 :nick "lislon"
                 :password lsn/gitter-pwd
                 :full-name "lislon"))
    "aig" (defun bb/gitter ()
            (interactive)
            (erc-tls :server "irc.gitter.im"
                     :port 6667
                     :nick "lislon"
                     :password lsn/gitter-pwd
                     :full-name "lislon"))
    "aif" (defun bb/freenode ()
            (interactive)
            (erc :server "irc.freenode.net"
                 :port "6667"
                 :nick "lislon"
                 :full-name "lislon")))

(when (configuration-layer/package-usedp 'erc)
  (defvar lsn/gitter-pwd "" "Password in local.el")
  (with-eval-after-load 'erc
    ;; IRC
    (add-hook 'erc-insert-pre-hook
              (defun bb/erc-foolish-filter (msg)
                "Ignores messages matching `erc-foolish-content'."
                (when (erc-list-match erc-foolish-content msg)
                  (setq erc-insert-this nil))))
    (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (erc-track-mode -1)
    (setq erc-insert-modify-hook
          '(erc-controls-highlight
            erc-button-add-buttons
            bb/erc-github-filter
            erc-fill
            erc-match-message
            erc-add-timestamp
            erc-hl-nicks))))
