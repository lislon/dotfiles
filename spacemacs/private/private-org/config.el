(setq org-directory "~/org")

(setq org-agenda-files (quote (
                               "~/org/todo.org"
                               "~/org/refile.org"
                               "~/org/hosting.org")))
(setq org-refile-use-outline-path t)

(setq org-capture-templates
(quote (("t" "todo" entry (file "~/org/refile.org")
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("b" "book" entry (file+headline "~/org/todo.org" "Book list")
         "* %x %?\n%U\n")
        ("m" "movie" entry (file+headline "~/org/todo.org" "Movie list")
         "* TODO %?\n%U\n")
        ("n" "note" entry (file "~/org/refile.org")
         "* %?\n%U\n%x")
        ("s" "snippet zsh" entry (file "~/org/refile.org")
         "* %?\n#+BEGIN_SRC sh\n%x\n#+END_SRC"))))

(global-set-key "\C-cc" 'org-capture)

;; Start agenda F1
(global-set-key (kbd "<f1>") 'org-agenda)

;; Org in on currently selected task (show menu with prefix)
(global-set-key (kbd "C-<f11>") 'org-clock-in)

;; Insert mode in capture mode
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Mobile
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-files '("~/org/todo.org"))

;; Encryption (ACP NullPointerException on android)
;; (setq org-mobile-use-encryption t)
;; (setq epa-armor t)
;; (setq org-mobile-encryption-password "password")

;; End of org config file
