(setq
 ;; ------------------------------------------------------------------------------
 ;; General
 ;; ------------------------------------------------------------------------------
 org-directory "~/org"

 org-agenda-files (quote (
                          "~/org/todo.org"
                          "~/org/refile.org"
                          "~/org/hosting.org"))

 org-special-ctrl-a/e t                  ; Ignore tags when editing headline
 auto-save-timeout 5                     ; Autosave for dropbox each 10 sec IDLE
 calendar-week-start-day 1               ; Start from monday

 ;; ------------------------------------------------------------------------------
 ;; Clocking
 ;; ------------------------------------------------------------------------------
 org-clock-out-remove-zero-time-clocks t ; Remove 0:00 times in :LOGBOOK:
 org-clock-remove-empty-clock-drawer t   ; Remove empty :LOGBOOK: drawlers due to 0:00 time
 org-clock-persist t                     ; Save last clock item after emacs is closed
 org-clock-in-resume t                   ; Resume clock if clock in task with not closed clock

 ;; ------------------------------------------------------------------------------
 ;; Refile
 ;; ------------------------------------------------------------------------------
 org-refile-use-outline-path t           ; Fuzzy match path when refile
 org-outline-path-complete-in-steps nil  ; Fuzzy match path when refile


 ;; Targets include this file and any file contributing to the agenda - up to 9
 ;; levels deep
 org-refile-targets (quote ((nil :maxlevel . 9)
                            ("~/org/diary.org" :maxlevel . 1)
                            (org-agenda-files :maxlevel . 9)))
 ;; ------------------------------------------------------------------------------
 ;; Mobile
 ;; ------------------------------------------------------------------------------
 org-mobile-directory "~/Dropbox/MobileOrg"
 org-mobile-files '(append org-agenda-files "~/org/refile.org")
 org-mobile-force-id-on-agenda-items nil


 ;; ------------------------------------------------------------------------------
 ;; Capture
 ;; ------------------------------------------------------------------------------
 org-capture-templates (quote (("t" "todo" entry (file "~/org/refile.org")
                                "* TODO %?\n%U\n" :clock-in t :clock-resume t)

                               ("g" "goals for today" entry (file+datetree "~/org/goal-today.org")
                                "* My Day\n** Briefing \n%?\n** Goals [/]\n - [ ] \n** Debriefing" :clock-in
                                t :clock-resume t)

                               ("b" "book" entry (file+headline "~/org/todo.org" "Book list")
                                "* %x %?\n%U\n")

                               ("m" "movie" entry (file+headline "~/org/todo.org" "Movie list")
                                "* TODO %?\n%U\n")

                               ("e" "english quote" item (file+headline "~/org/todo.org" "Just phrases")
                                "%?")

                               ("n" "note" entry (file "~/org/refile.org")
                                "* %?\n%U\n%x")

                               ("s" "snippet zsh" entry (file "~/org/refile.org")
                                "* %?\n#+BEGIN_SRC sh\n%x\n#+END_SRC")

                               ("a" "appointment" entry (file "~/org/refile.org")
                                                             "* APPOINTMENT with %?\nSCHEDULED %^T\n%U")

                               ("p" "pain" entry (file+headline "~/org/refile.org" "Pains")
                                "* TODO Pain to %?\nFILE: %f\nExample:\n#+BEGIN_SRC\n%i\n#+END_SRC\n%U")

                               ("i" "interruption")

                               ("ii" "default" entry (file+datetree "~/org/diary.org")
                                "* %?\n%U" :clock-in t :clock-resume t)

                               ("if" "food" entry (file+datetree "~/org/diary.org")
                                "* Lunch %? :lunch:\n%U" :clock-in t :clock-resume t)

                               ("it" "talk" entry (file+datetree "~/org/diary.org")
                                "* Talking with %?\n%U" :clock-in t :clock-resume t)

                               ("ie" "emacs learning" entry (file "~/org/refile.org")
                                "* TODO How do I %? in emacs :emacs:learning:\n%U" :clock-in t :clock-keep t)

                               ("iE" "emacs tuning" entry (file "~/org/refile.org")
                                "* TODO Making %? in emacs :emacs:\n%U" :clock-in t :clock-keep t)

                               ("ib" "blackhole" entry (file+datetree "~/org/diary.org")
                                "* Default task for the day\n%U" :clock-in t :clock-keep t)
                               ))

 ;; ------------------------------------------------------------------------------
 ;; Agenda
 ;; ------------------------------------------------------------------------------
 org-agenda-custom-commands (quote (("r" "Refile" tags "REFILE"
                                     ((org-agenda-overriding-header "Refile")
                                      (org-tags-match-list-sublevels nil)))
                                    (" " "Agenda"
                                     ((agenda "" nil))
                                     nil)))


 org-tag-alist '((:startgroup . nil)
                 ("@trans" . ?t)
                 ("@home" . ?h)
                 (:endgroup . nil)
                 (:startgroup . nil)
                 ("torwald". nil)
                 ("tatu". nil)
                 ("tema". nil)
                 ("denis". nil)
                 ("habr". nil)
                 (:endgroup . nil)
                 )
 ;; ------------------------------------------------------------------------------
 ;; Calendar & Time
 ;; ------------------------------------------------------------------------------
 calendar-latitude 59.95
 calendar-longitude 30.3
 calendar-time-zone (* 60 +3)
 calendar-standard-time-zone-name "MSK"
 calendar-time-display-form '(24-hours ":" minutes
                                       (if time-zone " (") time-zone (if time-zone ")"))

 ;; ------------------------------------------------------------------------------
 ;; TODO faces
 ;; ------------------------------------------------------------------------------
 org-todo-keyword-faces '(("PENDING" . "SaddleBrown") 
                          ("DOWNLD" . "LimeGreen")
                          ("INPROGR" . "Yellow"))
 )


;; C-C c capture
(global-set-key "\C-cc" 'org-capture)

;; Make C-c C-x C-i/o work everywhere (with prefix - clock in recent task)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)

(autoload 'org-clock-jump-to-current-clock "org-clock")
(global-set-key (kbd "C-c j") 'org-clock-jump-to-current-clock)

;; Org in on currently selected task (show menu with prefix)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;; (global-set-key (kbd "<f11>") 'org-clock-goto)


;; Insert mode in capture mode
(add-hook 'org-capture-mode-hook 'evil-insert-state)
;; Insert mode when adding headers
(add-hook 'org-insert-heading-hook 'evil-insert-state)

;; Auto fill
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Autosave all org & config file buffers for Dropbox
(defun auto-save-all-files ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (if (or (eq major-mode 'emacs-lisp-mode) (eq major-mode 'org-mode))
              (basic-save-buffer))
        ))))
(add-hook 'auto-save-hook 'auto-save-all-files)
;; Encryption (ACP NullPointerException on android)
;; (setq org-mobile-use-encryption t)
;; (setq epa-armor t)
;; (setq org-mobile-encryption-password "password")

(evil-leader/set-key-for-mode 'org-mode "of" 'helm-org-in-buffer-headings)

;; Shortcuts
;; SPC o o - todo.org
;; SPC o c - config.el
(evil-leader/set-key "oo" (defun my/jump-to-todo-file() (interactive) (find-file "~/org/todo.org")))
(evil-leader/set-key "oc" (defun my/jump-to-org-config-file () (interactive) (find-file "~/dotfiles/spacemacs/private/my-org/config.el")))
(evil-leader/set-key "oa" 'org-agenda)
(evil-leader/set-key "oC" 'org-capture)

;; Resume clocking task when emacs is restarted
(with-eval-after-load 'org
  (progn
    (org-clock-persistence-insinuate)
    (org-clock-load)))


(evil-leader/set-key-for-mode 'org-mode "mtia" 'my/org-append-row-to-table)
;; (save-current-buffer
;;     (save-excursion
;;     (switch-to-buffer "*org*")
;;     (org-mode)
;;     ))

;; Crypt facilities
(require 'epa-file)
(epa-file-enable)

(eval-after-load "org"
  '(progn
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (sh . t)
     (lisp . t)
     ))
  ;; Auto insert when ~TAB` in tables
  (advice-add 'org-table-next-field :after 'evil-insert-state)
  ))
;; End of org config file
