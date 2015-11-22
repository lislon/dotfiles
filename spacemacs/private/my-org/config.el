(setq-default
 ;; ------------------------------------------------------------------------------
 ;; Lord-lislon board
 ;; ------------------------------------------------------------------------------
 my/lord-lislon-tag "lislon"

 ;; ------------------------------------------------------------------------------
 ;; General
 ;; ------------------------------------------------------------------------------
 org-directory "~/org"

 org-agenda-files (quote (
                          "~/org/todo.org"
                          "~/org/tasks.org"
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
 org-clock-idle-time 3
 org-clock-x11idle-program-name "xprintidle"
 org-x11idle-exists-p t                 ; When emacs started as daemon, x11 not initialized


 ;; ------------------------------------------------------------------------------
 ;; Refile
 ;; ------------------------------------------------------------------------------
 org-refile-use-outline-path t           ; Fuzzy match path when refile
 org-outline-path-complete-in-steps nil  ; Fuzzy match path when refile


 ;; Targets include this file and any file contributing to the agenda - up to 9
 ;; levels deep
 org-refile-targets (quote ((nil :maxlevel . 9)
                            ("~/org/diary.org" :maxlevel . 1)))

 ;; Mobile
 ;; ------------------------------------------------------------------------------
 org-mobile-directory "~/Dropbox/MobileOrg"
 org-mobile-files '(append org-agenda-files
                           "~/org/refile.org"
                           "~/org/books.org")
 org-mobile-force-id-on-agenda-items nil


 ;; ------------------------------------------------------------------------------
 ;; Capture
 ;; ------------------------------------------------------------------------------
 org-capture-templates (quote (("t" "todo" entry (file "~/org/refile.org")
                                "* TODO %^{Todo}\n%U" :clock-in t :clock-resume t
                                :immediate-finish t)

                               ("T" "custom todo")

                               ("Tt" "Tag todo" entry (file "~/org/refile.org")
                                "* TODO %^{Todo} %^G\n%U\n" :clock-in t :clock-resume t
                                :immediate-finish t)

                               ("Tl" "Learning" entry (file "~/org/refile.org")
                                "* TODO %^{Todo} %^G:learning:\n%U\n" :clock-in t :clock-resume t
                                :immediate-finish t)

                               ("Td" "goals for today" entry (file "~/org/refile.org")
                                "* TODO %^{Goal}\n%U\nSCHEDULED: %t ")

                               ("b" "book" entry (file+headline "~/org/todo.org" "Book list")
                                "* PENDING %^{Book} %?\n%U\n")

                               ("m" "movie" entry (file+headline "~/org/todo.org" "Movie list")
                                "* TODO %?\n%U\n")

                               ("s" "share" entry (file+headline "~/Dropbox/lord-lislon/lord-lislon.org" "Shared")
                                "* %^{Title} :lislon:\n%U\n%?")

                               ("e" "English quote" item (file+headline "~/org/todo.org" "Just phrases")
                                "%^{quote}" :immediate-finish t)

                               ("r" "Russian quote" item (file+headline "~/org/todo.org" "Russian")
                                "* %^{quote}\n%?")

                               ("n" "note" entry (file "~/org/refile.org")
                                "* %^{Title}\n%U\n%?")

                               ("s" "snippet zsh" entry (file "~/org/refile.org")
                                "* %?\n#+BEGIN_SRC sh\n%x\n#+END_SRC")

                               ("a" "appointment" entry (file "~/org/refile.org")
                                "* APPOINTMENT with %?\nSCHEDULED %^T\n%U")

                               ("p" "pain" entry (file+headline "~/org/tasks.org" "Pains")
                                "* TODO Pain to %^{Pain}\n%U\n%?")

                               ("i" "interruption")

                               ("ii" "default" entry (file+datetree "~/org/diary.org")
                                "* %^{Interruption} %^G\n%U" :clock-in t :clock-keep t)

                               ("iy" "youtube" entry (file+datetree "~/org/diary.org")
                                "* %^{Interruption} %^G:youtube:\n%U" :clock-in t :clock-keep t
                                :immediate-finish t)

                               ("it" "talk" entry (file+datetree "~/org/diary.org")
                                "* Talking with %^{Whom} :talk:\n%U" :clock-in t :clock-resume t)

                               ("ik" "kitchen" entry (file+datetree "~/org/diary.org")
                                "* go to kitchen%?\n%U" :clock-in t :clock-resume t)

                               ("is" "shower" entry (file+datetree "~/org/diary.org")
                                "* Shower :shower:\n%U"
                                :clock-in t :clock-resume t)

                               ("iw" "WC" entry (file+datetree "~/org/diary.org")
                                "* WC :wc:\n%U"
                                :clock-in t :clock-resume t)

                               ("il" "learning" entry (file+datetree "~/org/diary.org")
                                "* Learning %^{Subject} :learning:\n%U\n%?"
                                :clock-in t :clock-keep t)

                               ("ie" "emacs idea" entry (file "~/org/refile.org")
                                "* TODO %^{Idea} :emacs:idea:\n%U" :clock-in t :clock-resume t
                                :immediate-finish t)

                               ("im" "modify system" entry (file "~/org/refile.org")
                                "* TODO %^{Title} %^G\n%U" :clock-in t :clock-keep t
                                :immediate-finish t)

                               ("ig" "googling" entry (file "~/org/refile.org")
                                "* Googling %^{Title} :google:%^G\n%U" :clock-in t :clock-keep t
                                :immediate-finish t)

                               ("ib" "blackhole" entry (file+datetree "~/org/diary.org")
                                "* Default task for the day\n%U" :clock-in t :clock-keep t)

                               ("l" "Default template" entry (file+headline "~/org/refile.org")
                                "* %c\n%u\n%i"
                                :empty-lines 1)
                               ))

 ;; ------------------------------------------------------------------------------
 ;; Agenda
 ;; ------------------------------------------------------------------------------
 org-agenda-skip-scheduled-if-done t    ; Do not shot DONE items
 org-agenda-skip-deadline-if-done t

 org-agenda-custom-commands (quote (("r" "Refile" tags "REFILE"
                                     ((org-agenda-overriding-header "Refile")
                                      (org-tags-match-list-sublevels nil)))

                                    ("b" "Books" todo "INPROGR"
                                     ((org-agenda-overriding-header "Books I am reading")
                                      (org-agenda-sorting-strategy '(user-defined-down))
                                      (org-agenda-cmp-user-defined 'my/org-sort-agenda-logbook)
                                      (org-agenda-files '("~/org/todo.org"))))

                                    ("y" "Todo things"
                                     (
                                      ;; + reading
                                      (tags-todo "TODO={INPROGR\\|DOWNLD}+BOOK"
                                                 ((org-agenda-overriding-header "Reading")
                                                  (org-agenda-prefix-format "Reading ")
                                                  ))
                                      ;; - training/practicing
                                      ;; (tags-todo "+emacs"
                                      ;;            ((org-agenda-overriding-header "")
                                      ;;             (org-agenda-prefix-format "Emacs ")))
                                      ;; shopping
                                      (tags-todo "+shopping-errands"
                                                 ((org-agenda-overriding-header "E-shopping")
                                                  (org-agenda-prefix-format "Shopping ")))
                                      ;; household
                                      (tags-todo "+household"
                                                 ((org-agenda-overriding-header "Housekeeping")
                                                  (org-agenda-prefix-format "Household ")))
                                      ;; movie/youtube
                                      (tags-todo "+video"
                                                 ((org-agenda-overriding-header "Watching movies")
                                                  (org-agenda-prefix-format "Movie ")))
                                      ;; research/googling
                                      (tags-todo "+googling"
                                                 ((org-agenda-overriding-header "Googling")
                                                  (org-agenda-prefix-format "Google ")))
                                      ;; emacs tuning
                                      (tags-todo "emacs"
                                                 ((org-agenda-overriding-header "Emacs tuning")
                                                  (org-agenda-prefix-format "Emacs ")
                                                  (org-agenda-sorting-strategy '(time-up)) ))
                                      ;; system tuning
                                      (tags-todo "linux-learning"
                                                 ((org-agenda-overriding-header "System tuning")
                                                  (org-agenda-prefix-format "OS ")))
                                      ;; learning
                                      (tags-todo "learning"
                                                 ((org-agenda-overriding-header "Gain skills")
                                                  (org-agenda-prefix-format "Lean ")))
                                      ;; sysadmin chroes
                                      (tags-todo "linux&chore"
                                                 ((org-agenda-overriding-header "OS chores")
                                                  (org-agenda-prefix-format "OS Chore ")))
                                      ;; organizing
                                      (tags-todo "+org"
                                                 ((org-agenda-overriding-header "Organize")
                                                  (org-agenda-prefix-format "Org ")))
                                      ;; (org-agenda-files '("~/org/todo.org"
                                      ;;                     "~/org/refile.org"
                                      ;;                     "~/org/tasks.org"))
                                      ;; - training/practicing
                                      ) (
                                         (org-agenda-sorting-strategy '(time-down))
                                         ))
                                    ;; + shopping
                                    ;; household chores
                                    ;; movie/youtube
                                    ;; research/googling
                                    ;; emacs tuning
                                    ;; system tuning/scripting
                                    ;; learning
                                    ;; sysadmin chores
                                    ;; organizing

                                    (" " "Agenda"
                                     (
                                      (agenda "" nil)
                                      (tags (concat "-" my/lord-lislon-tag "+LEVEL=2+TIMESTAMP_IA>=\"<-7d>\"")
                                            ((org-agenda-overriding-header "Lord/Lislon news (7 days)")
                                             (org-agenda-prefix-format "     ")
                                             (org-agenda-sorting-strategy '(timestamp-up))
                                             (org-agenda-files '("~/Dropbox/lord-lislon/lord-lislon.org")))
                                            )
                                      )
                                     nil)))


 org-tag-alist '(;; ("@trans" . ?t)
                 ;; ("@home" . ?h)
                 ("arch" . ?a)
                 ("emacs" . ?e)
                 ("linux" . ?l)
                 ("learning" . ?L)
                 ("googling" . ?g)
                 ("reading" . ?r)
                 ;; (:startgroup . nil)
                 ;; ("torwald". nil)
                 ;; ("tatu". nil)
                 ;; ("tema". nil)
                 ;; ("denis". nil)
                 ;; ("habr". nil)
                 ;; (:endgroup . nil)
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

 ;;------------------------------------------------------------------------------
 ;; pomodoro
 ;; ------------------------------------------------------------------------------
 org-pomodoro-length 45
 org-pomodoro-short-break-length 15

 ;; ------------------------------------------------------------------------------
 ;; TODO faces
 ;; ------------------------------------------------------------------------------
 org-todo-keyword-faces '(("PENDING" . "SaddleBrown") 
                          ("DOWNLD" . "LimeGreen")
                          ("INPROGR" . "Yellow"))
 )


;; C-C c capture
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "<f1>") 'org-agenda)

;; Make C-c C-x C-i/o work everywhere (with prefix - clock in recent task)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-x") 'org-clock-in-last)
(global-set-key (kbd "<f12>") (defun my/org-clockin-recent-tasks() (interactive)
                                (org-clock-in '(4))))

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
(evil-leader/set-key "oe" (defun my/jump-to-org-tasks-file () (interactive) (find-file "~/org/tasks.org")))
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



;; Org capture protocol
(eval-after-load "org" '(require 'org-protocol))
(add-hook 'org-capture-mode-hook 'my/org-capture-delete-other-windows)
(advice-add 'org-protocol-do-capture :around 'my/intercept-make-capture-frame)


;; Crypt facilities
;; (require 'epa-file)
;; (epa-file-enable)

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

;; ------------------------------------------------------------------------------
;; Update emacs agenda file for awesomewm
;; ------------------------------------------------------------------------------

;; update agenda file after changes to org files
;; (defun th-org-mode-init ()
;;   (add-hook 'after-save-hook 'th-org-update-agenda-file t t))

;; (add-hook 'org-mode-hook 'th-org-mode-init)

;; ;; that's the export function
;; (defun th-org-update-agenda-file (&optional force)
;;   (interactive)
;;   (save-excursion
;;     (save-window-excursion
;;       (let ((file "/tmp/org-agenda.txt"))
;;         (org-agenda-list)
;;         (org-agenda-write file)))))

;; ;; do it once at startup
;; (th-org-update-agenda-file t)

;; End of org config file
