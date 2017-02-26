(setq-default
 ;; ------------------------------------------------------------------------------
 ;; Lord-lislon board
 ;; ------------------------------------------------------------------------------
 my/lord-lislon-tag "lislon"

 ;; ------------------------------------------------------------------------------
 ;; General
 ;; ------------------------------------------------------------------------------
 org-directory "~/org"

 org-agenda-files '("~/org/dynamic/"
                   "~/org/dynamic/projects/")

 diary-file "~/org/diary.txt"
 calendar-date-style 'iso
 org-special-ctrl-a/e t                  ; Ignore tags when editing headline
 auto-save-timeout 5                     ; Autosave for dropbox each 10 sec IDLE
 calendar-week-start-day 1               ; Start from Monday
 org-agenda-span 'day                    ; View only today, so other days not distract me
 org-agenda-start-on-weekday nil         ; When viewing view start from current day
 org-agenda-text-search-extra-files '(agenda-archives)
 org-todo-keywords '((sequence "TODO" "|" "DONE" "REJT"))
 org-list-allow-alphabetical t
 org-table-default-size "2x5"           ; Default 2 columns table

 ;;------------------------------------------------------------------------------
 ;;Clocking
 ;;------------------------------------------------------------------------------
 org-clock-out-remove-zero-time-clocks t ; Remove 0:00 times in :LOGBOOK:
 org-clock-remove-empty-clock-drawer t   ; Remove empty :LOGBOOK: drawlers due to 0:00 time
 org-clock-persist t                     ; Save last clock item after emacs is closed
 org-clock-in-resume t                   ; Resume clock if clock in task with not closed clock
 org-clock-idle-time 3
 org-clock-x11idle-program-name "xprintidle"
 org-clock-mode-line-total 'today
 org-x11idle-exists-p t                 ; When emacs started as daemon, x11 not initialized
 org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 5:00")))
 org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"


 ;; ------------------------------------------------------------------------------
 ;; Refile
 ;; ------------------------------------------------------------------------------
 org-refile-use-outline-path 'file           ; Fuzzy match path when refile
 org-outline-path-complete-in-steps nil      ; Fuzzy match path when refile

 ;; Archive
 org-archive-location "~/org/archive/%s_archive::"


 ;; Targets include this file and any file contributing to the agenda - up to 9
 ;; levels deep
 org-refile-targets (quote ((nil :maxlevel . 9)
                            (org-agenda-files :maxlevel . 9)
                            ("~/org/dynamic/computers.org" :maxlevel . 2)
                            ("~/org/dynamic/programming.org" :maxlevel . 1)
                            ("~/org/dynamic/todo-someday.org" :level . 1)
                            ("~/org/dynamic/notes.org" :level . 0)
                            ("~/org/static/diary.org" :maxlevel . 1)
                            ("~/org-shared/static/programming.org" :maxlevel . 1)
                            ("~/org-shared/static/java.org" :maxlevel . 1)))


 ;; ------------------------------------------------------------------------------
 ;; Edit
 ;; ------------------------------------------------------------------------------
 ;; I do not want splitting while editing header
 org-M-RET-may-split-line '((default . nil) (item . nil))
 org-catch-invisible-edits 'show-and-error         ; let's test this optio

 ;; ------------------------------------------------------------------------------
 ;; Export
 ;; ------------------------------------------------------------------------------

 ;; ------------------------------------------------------------------------------
 ;; Capture
 ;; ------------------------------------------------------------------------------
 org-capture-templates (quote (("t" "todo" entry (file+headline "~/org/dynamic/tasks.org" "Simple")
                                "* TODO %^{Todo}\n%U\n%?" :clock-in t :clock-resume t
                                :immediate-finish nil)

                               ("T" "Tag todo" entry (file+headline "~/org/dynamic/tasks.org" "Simple")
                                "* TODO %^{Todo} %^G\n%U\n%?" :clock-in t :clock-resume t
                                :immediate-finish nil)

                               ("d" "todo today" entry (file+headline "~/org/dynamic/today.org" "Simple")
                                "* TODO %^{Todo today}\n%U\nSCHEDULED: %t" :clock-in t :clock-resume t
                                :immediate-finish nil)

                               ("h" "home todo" entry (file+headline "~/Dropbox/workorg/home.org" "Todo")
                                "* TODO %^{Todo}\n%U"
                                :immediate-finish nil)

                               ("p" "Programming")

                               ("pj" "java" entry (file+headline "~/org-shared/static/java.org" "Java")
                                "* %^{Java topic}\n%U\n%?")

                               ("pp" "common" entry (file+headline "~/org-shared/static/programming.org" "Java")
                                "* %^{Topic}\n%U\n%?")

                               ("D" "Date todo" entry (file+headline "~/org/dynamic/tasks.org" "Simple")
                                "* TODO %^{Todo}\n%U\n%T" :clock-in t :clock-resume t
                                :immediate-finish nil)
                               ;; ("L" "Learning" entry (file "~/org/refile.org")
                               ;;  "* TODO %^{Todo} %^G:learning:\n%U\n" :clock-in t :clock-resume t
                               ;;  :immediate-finish t)

                               ("B" "book" entry (file+headline "~/org/dynamic/todo.org" "Books")

                                "* PENDING %^{Book}\n%U\n%?")

                               ("o" "organization note" entry (file+headline "~/org/dynamic/todo.org" "Organization my life")
                                "* %^{Organization title}\n%U\n%?")

                               ("g" "goal" entry (file+headline "~/org/goal-today.org" "Task stack")
                                "* TODO %^{Title}\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n" :immediate-finish t :prepend t :clock-in t :clock-keep t)
                               ("G" "goal later" entry (file+headline "~/org/goal-today.org" "Task stack")
                                "* TODO %^{Title}\n:PROPERTIES:\n:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n:END:\n" :prepent t)

                               ("M" "movie" entry (file+headline "~/org/dynamic/todo.org" "Movies")
                                "* TODO %?\n%U\n")

                               ("s" "share" entry (file+headline "~/Dropbox/lord-lislon/lord-lislon.org" "Shared")
                                "* %^{Title} :lislon:\n%U\n%?")

                               ("e" "English quote" item (file+headline "~/org/dynamic/todo.org" "Just phrases")
                                "%^{quote}" :immediate-finish t)

                               ("E" "English question" entry (file+headline "~/org/dynamic/english.org" "Questions")
                                "* TODO %^{Title}\n%U\n%F\n%?\n#+BEGIN_SRC\n%i\n#+END_SRC")

                               ("q" "Russian quote" item (file+headline "~/org/dynamic/todo.org" "Russian")
                                "* %^{quote}\n%?")

                               ("n" "note" entry (file "~/org/dynamic/notes.org")
                                "* %^{Title}\n%U\n%?")

                               ;; ("p" "note" entry (file+headline "~/org/dynamic/computers.org" "Problem solving")
                               ;;  "* %^{Title}\n%U\n%?")

                               ("z" "snippet zsh" entry (file+headline "~/org/dynamic/todo.org" "Bash/Zsh")
                                "* %?\n#+BEGIN_SRC sh\n%x\n#+END_SRC")

                               ("m" "migration" entry (file+headline "~/org/static/migration.org" "Tools Migration")
                                "* %^{Old} -> %^{New}\n%U\nReason: %?")

                               ("a" "appointment" entry (file+headline "~/org/dynamic/tasks.org" "Events")
                                "* APPOINTMENT with %?
:PROPERTIES:
:APPT_WARNTIME: 90
:END:
SCHEDULED %^T
%U")

                               ("b" "buy" entry (file+headline "~/org/dynamic/buy.org" "Buy")
                                "* TODO Buy %^{Buy} :errands:buy:\n%U" :immediate-finish t)

                               ("p" "pain" entry (file+headline "~/org/dynamic/tasks.org" "Pains")
                                "* TODO %^{Activity} / %^{Pain}\n%U\n%?")

                               ("i" "interruption")

                               ("ii" "default" entry (file+datetree "~/org/static/diary.org")
                                "* %^{Interruption} %^G\n%U" :clock-in t :clock-keep t)

                               ("iy" "youtube" entry (file+datetree "~/org/static/diary.org")
                                "* %^{Interruption} %^G:youtube:\n%U" :clock-in t :clock-keep t
                                :immediate-finish t)

                               ("it" "talk" entry (file+datetree "~/org/static/diary.org")
                                "* Talking with %^{Whom} :talk:\n%U" :clock-in t :clock-keep t)

                               ("ik" "kitchen" entry (file+datetree "~/org/static/diary.org")
                                "* go to kitchen%?\n%U" :clock-in t :clock-resume t)

                               ("is" "shower" entry (file+datetree "~/org/static/diary.org")
                                "* Shower :shower:\n%U"
                                :clock-in t :clock-resume t)

                               ("in" "News" entry (file+datetree "~/org/static/diary.org")
                                "* Reading news :news:\n%U"
                                :clock-in t :clock-resume t)

                               ("iw" "WC" entry (file+datetree "~/org/static/diary.org")
                                "* WC :wc:\n%U"
                                :clock-in t :clock-resume t)

                               ("il" "learning" entry (file+datetree "~/org/static/diary.org")
                                "* Learning %^{Subject} :learning:\n%U\n%?"
                                :clock-in t :clock-keep t)

                               ("ie" "emacs idea" entry (file "~/org/dynamic/refile.org")
                                "* TODO %^{Idea} :emacs:idea:\n%U" :clock-in t :clock-resume t
                                :immediate-finish t)

                               ("im" "modify system" entry (file "~/org/dynamic/refile.org")
                                "* TODO %^{Title} %^G\n%U" :clock-in t :clock-keep t
                                :immediate-finish t)

                               ("ig" "googling" entry (file "~/org/dynamic/refile.org")
                                "* Googling %^{Title} :google:%^G\n%U" :clock-in t :clock-keep t
                                :immediate-finish t)

                               ("ib" "blackhole" entry (file+datetree "~/org/static/diary.org")
                                "* Default task for the day\n%U" :clock-in t :clock-keep t)

                               ("l" "Default template" entry (file "~/org/dynamic/refile.org")
                                "* %c\n%u\n%i"
                                :empty-lines 1)
                               ))

 ;; ------------------------------------------------------------------------------
 ;; Agenda
 ;; ------------------------------------------------------------------------------
 org-agenda-skip-scheduled-if-done nil    ; Do not shot DONE items
 org-agenda-skip-deadline-if-done nil
 org-deadline-warning-days 5            ; Warn about deadline

 org-agenda-custom-commands (quote (("r" "Refile" tags "REFILE"
                                     ((org-agenda-overriding-header "Refile")
                                      (org-tags-match-list-sublevels nil)))

                                    ("B" "Books" todo "INPROGR"
                                     ((org-agenda-overriding-header "Books I am reading")
                                      (org-agenda-sorting-strategy '(user-defined-down))
                                      (org-agenda-cmp-user-defined 'my/org-sort-agenda-logbook)
                                      (org-agenda-files '("~/org/dynamic/todo.org"))))

                                    ("y" "Todo things"
                                     (
                                      ;; + reading
                                      (tags-todo "TODO={INPROGR\\|DOWNLD}+book"
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
                                      (tags-todo "+movie"
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
                                      (tags-todo "linux-learning-emacs"
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
                                      ;; uncategorized
                                      (tags-todo "-linux-emacs-learning-org-BOOK-errands"
                                                 ((org-agenda-overriding-header "Rest")
                                                  (org-agenda-prefix-format "Rest ")))
                                      ;; (org-agenda-files '("~/org/todo.org"
                                      ;;                     "~/org/refile.org"
                                      ;;                     "~/org/dynamic/tasks.org"))
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
                                      (tags "+lord-lislon+TIMESTAMP_IA>=\"<-7d>\""
                                            ((org-agenda-overriding-header "Lord/Lislon news (7 days)")
                                             (org-agenda-prefix-format "     ")
                                             (org-agenda-sorting-strategy '(timestamp-down))
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
                 ("movie" . ?m)
                 ("book" . ?b)
                 ("lislon" . ?i)
                 ("reading" . ?r)
                 ("household" . ?h)
                 ;; (:startgroup . nil)
                 ;; ("torwald". nil)
                 ;; ("tatu". nil)
                 ;; (:endgroup . nil)
                 )
 ;; ------------------------------------------------------------------------------
 ;; appt reminders
 ;; ------------------------------------------------------------------------------
 appt-message-warning-time 60
 ;appt-display-interval 20

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
 org-pomodoro-length 20
 org-pomodoro-short-break-length 5

 ;;;------------------------------------------------------------------------------
 ;;; babel
 ;;; ------------------------------------------------------------------------------
 org-confirm-babel-evaluate t
 my-org-babel-languages '(
                          (sh . t)
                          (lisp . t)
                          (emacs-lisp . t)
                          (java . t)
                          (plantuml . t)
                          )
 ;;------------------------------------------------------------------------------
 ;; UML
 ;; ------------------------------------------------------------------------------
 org-plantuml-jar-path (if (file-exists-p "/opt/plantuml/plantuml.jar") "/opt/plantuml/plantuml.jar" "")

 ;; ------------------------------------------------------------------------------
 ;; TODO faces
 ;; ------------------------------------------------------------------------------
 org-todo-keyword-faces '(("PENDING" . "SaddleBrown")
                          ("DOWNLD" . "LimeGreen")
                          ("INPROGR" . "Yellow")
                          ("REJT" . "SlateGray")
                          ("APPOINTMENT" . "DarkViolet"))
 )


;; C-C c capture
;; (global-set-key "\C-cc" 'org-capture)  ;; Spacemacs by default use C-c c
(global-set-key (kbd "<f1>") 'org-agenda)
;; Make C-c C-x C-i/o work everywhere (with prefix - clock in recent task)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-x") 'org-clock-in-last)
(global-set-key (kbd "C-c C-o") 'org-open-at-point)
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
;; (add-hook 'org-insert- 'evil-insert-state)

;; Auto fill for org and all text-mode hooks
(add-hook 'text-mode-hook 'auto-fill-mode)

(add-hook 'org-timer-done-hook 'my/org-timer-done)

(defun my/org-timer-done ()
  "Functions called when timer is done"
  (if tea-time-sound
      (if tea-time-sound-command
          (start-process-shell-command "tea-ready" nil (format tea-time-sound-command tea-time-sound))
        (play-sound-file tea-time-sound))
    (progn (beep t) (beep t))))


;; Encryption (ACP NullPointerException on android)

;; void-function bind-map error:
;;(evil-leader/set-key-for-mode 'org-mode "of" 'helm-org-in-buffer-headings)

;; Shortcuts
(defmacro my/set-key-file-link (key file)
  `(evil-leader/set-key ,key
     (defun ,(make-symbol
              (concat "my/jump-to-" (file-name-base file) "-file"))()
       (interactive)
       (find-file ,file))))

(my/set-key-file-link "oC" "~/dotfiles/spacemacs/private/my-org/config.el")
(my/set-key-file-link "oo" "~/org/dynamic/todo.org")
(my/set-key-file-link "oh" "~/Dropbox/workorg/home.org")
(my/set-key-file-link "oc" "~/org/dynamic/computers.org")
(my/set-key-file-link "ok" "~/org/dynamic/keys.org")
(my/set-key-file-link "ot" "~/org/dynamic/tasks.org")
(my/set-key-file-link "or" "~/org/dynamic/refile.org")
(my/set-key-file-link "on" "~/org/dynamic/notes.org")
(my/set-key-file-link "oj" "~/org-shared/static/java.org")
(my/set-key-file-link "oB" "~/org/static/books.org")
(my/set-key-file-link "ob" "~/org/dynamic/buy.org")
(my/set-key-file-link "ou" "~/org/static/coubs.org")

(evil-leader/set-key "bo" (defun my/make-org-buffer() (interactive)
                                 (spacemacs/new-empty-buffer)
                                 (org-mode)
                                 ))
;; (evil-define-key 'normal evil-org-mode-map "gp" nil)

;; not using
;(global-set-key (kbd "<f5>") (lambda () (interactive) (org-capture '() "g")))
;(global-set-key (kbd "S-<f5>") (lambda () (interactive) (org-capture '() "G")))
;(global-set-key (kbd "C-<f5>") 'org-clock-jump-to-current-clock)
;(global-set-key (kbd "M-<f5>") (lambda () (interactive) (find-file "~/org/goal-today.org")))

;; Resume clocking task when emacs is restarted
(with-eval-after-load 'org
  (progn
    (org-clock-persistence-insinuate)
    (org-clock-load)))

;; commented because of error during ~SPC f e R~ while: Key sequence m t i a starts with non-prefix key m
;; (evil-leader/set-key-for-mode 'org-mode "mtia" 'my/org-append-row-to-table)

;; (save-current-buffer
;;     (save-excursion
;;     (switch-to-buffer "*org*")
;;     (org-mode)
;;     ))



;; Org capture protocol
(with-eval-after-load "org"
  (require 'org-protocol)
  (add-hook 'org-capture-mode-hook 'my/org-capture-delete-other-windows)
  (advice-add 'org-protocol-do-capture :around 'my/intercept-make-capture-frame)
  (advice-add 'org-insert-heading :before 'my/org-insert-heading-advice)
  )

;; ------------------------------------------------------------------------------
;; lord-lislon hooks
;; ------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist
             '("lord-lislon\\.org\\'" . (lambda ()
                                          (message "mimi")
                                          (org-mode)
                                          (add-hook 'org-insert-heading-hook 'my/lord-lislon-auto-insert-date-heading nil t))))

;; Crypt facilities
;; (require 'epa-file)
;; (epa-file-enable)

(with-eval-after-load "org"
  (org-babel-do-load-languages 'org-babel-load-languages my-org-babel-languages)
  ;; Auto insert when ~TAB` in tables
  (advice-add 'org-table-next-field :after 'evil-insert-state))

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

(advice-add 'org-metaright :around #'my/org-metaright-or-evil-shift-right-advice)
(advice-add 'org-metaleft :around #'my/org-metaleft-or-evil-shift-left-advice)
(advice-add 'org-metareturn :around #'my/org-metaleft-or-evil-shift-left-advice)

;; Auto insert when press C-SPC in tables for clear field
(advice-add 'org-table-blank-field :after (lambda (&rest r) (evil-insert-state)))


;; Commented for optimization tests
 (with-eval-after-load 'org
   ;; emacs 24.4 No such file or directory
   ;; (require 'org-notify)                 ; Support only deadlines
   ;; (org-notify-start)
   ;; how deadline worked?
   (appt-activate 1)

  ;; update appt each time agenda opened
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

  ;; our little façade-function for djcb-popup
  (defun djcb-appt-display (min-to-app new-time msg)
    (djcb-popup (format "Appointment in %s minute(s)" min-to-app) msg
                "~/confiles/linux/icons/apppointment-grey-32x32.png"

                "~/confiles/linux/sounds/choir-a.wav"))
  (setq appt-disp-window-function 'djcb-appt-display)
  (defadvice org-agenda-to-appt (before wickedcool activate)
    "Clear the appt-time-msg-list."
    (setq appt-time-msg-list nil))
  )

(eval-after-load "helm-files"
  '(setq helm-source-recentf
        (helm-make-source "Recentf" 'helm-recentf-source
          :filtered-candidate-transformer nil
          )))


(add-to-list 'auto-mode-alist
             '("goal-today\\.org\\'" .
               (lambda ()
                 (add-hook 'org-after-todo-state-change-hook 'my/org-goal-today-after-done nil t)
                 (spacemacs/set-leader-keys-for-major-mode 'org-mode
                   "oD" 'my/org-goal-today-finish-day)
                 )))


;; Natural help help

(add-hook 'org-agenda-mode-hook (lambda ()
(define-key org-agenda-keymap (kbd "q") (lambda ()
                                          "Fast exit from agenda in special frame"
                                          (interactive)
                                          (when (equal "agenda" (frame-parameter nil 'name))
                                            (delete-frame))))
                                  ;; (if f)
                                  ;; (define-key overriding-local-map (kbd "v") 'org-agenda-view-mode-dispatch )
                                  ;; (my/redefine-evilified-key org-agenda-keymap (kbd "C-h") nil)
                                  ))

;; (my/redefine-evilified-key org-agenda-keymap (kbd "v") nil) ;; not works for some reason :(


;; (defun org-mode-hook-fix-agenda-keys ()
;;   ;; make v button work in agenda
;;   (define-key evil-evilified-state-map (kbd "v") nil))

;; (add-hook 'org-mode-hook 'org-mode-hook-fix-agenda-keys)

(setq-default
 org-mobile-directory "~/Dropbox/MobileOrg"
 org-mobile-files '("~/org/dynamic/"
                    "~/org/static/books.org"
                    "~/org/archive/books.org_archive"
                    )
 org-mobile-force-id-on-agenda-items nil)
(defun my/org-mobile-fix-index-bug ()
  "Fixes MobileOrg's index.org after push to workaround bug in Android.
That function deletes \"#+ALLPRIORITIES\" line from index.org file"
  (interactive)
  (let ((file (concat org-mobile-directory "/index.org")))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (search-forward "#+ALLPRIORITIES" nil t)
          ;; Avoid polluting kill-ring by not calling (kill-line)
          (let ((beg (progn (forward-line 0)
                            (point))))
            (forward-line 1)
            (delete-region beg (point))))
        (write-region nil nil file)))))

(advice-add 'org-mobile-push :after 'my/org-mobile-fix-index-bug)
