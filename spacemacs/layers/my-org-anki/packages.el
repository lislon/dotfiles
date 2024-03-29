;; https://github.com/eyeinsky/org-anki
;; (setq my-org-packages '(noflet
;;                         f
;;                         org
;;                         plantuml-mode
;;                         (org-protocol-capture-html
;;                          :location (recipe
;;                                     :fetcher github
;;                                     :repo "alphapapa/org-protocol-capture-html"))
;;                         (helm-yandex-geoapi :location local)
;;                         visual-fill-column
;;                         (artist :location built-in)))

;; (defun my-org/init-f ()
;;   (message "my-org/post-init-f")
;;   (use-package f
;;     :demand t)
;;   )

;; (defun my-org/init-visual-fill-column ()
;;   ;; (message "my-org/post-init-f")
;;   ;; (use-package visual-fill-column
;;     ;; :demand t)
;;   )

;; (defvar my-org/screenshot-dir "~/Pictures/Screenshots" "Directory with latest captured images")

;; (defun my-org/init-org-protocol-capture-html ()
;;   (use-package org-protocol-capture-html))

;; (defun my-org/init-plantuml-mode ()
;;   (message "my-org/init-plantuml-mode")
;;   (use-package plantuml-mode
;;     :init
;;     (setq plantuml-java-args "")
;;     :config
;;     (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;;     )
;;   )

;; (defun my-org/post-init-org ()
;;   ;; (message "my-org/post-init-org")
;;   ;; (package-initialize)
;;   ;; (message "my-org/post-init-org2")
;;   (setq-default
;;    ;; ------------------------------------------------------------------------------
;;    ;; General
;;    ;; ------------------------------------------------------------------------------
;;    org-directory "~/OneDrive/org"

;;    org-agenda-files '("~/OneDrive/org/dynamic/"
;;                       "~/OneDrive/org/dynamic")

;;    diary-file "~/OneDrive/org/diary.txt"
;;    calendar-date-style 'iso
;;    org-special-ctrl-a/e t                  ; Ignore tags when editing headline
;;    auto-save-timeout 5                     ; Autosave for dropbox each 10 sec IDLE
;;    calendar-week-start-day 1               ; Start from Monday
;;    org-agenda-span 'day                    ; View only today, so other days not distract me
;;    org-agenda-start-on-weekday nil         ; When viewing view start from current day
;;    org-agenda-text-search-extra-files '(agenda-archives)
;;    org-todo-keywords '((sequence "TODO" "|" "DONE" "REJT"))
;;    org-list-allow-alphabetical t
;;    org-table-default-size "2x5"           ; Default 2 columns table
;;    org-link-frame-setup  '((file . find-file))
;;    org-ditaa-jar-path "~/OneDrive/dotfiles/spacemacs/scripts/ditaa.jar"
;;    org-return-follows-link t
;;    ;;------------------------------------------------------------------------------
;;    ;;Clocking
;;    ;;------------------------------------------------------------------------------
;;    org-clock-out-remove-zero-time-clocks t ; Remove 0:00 times in :LOGBOOK:
;;    org-clock-remove-empty-clock-drawer t   ; Remove empty :LOGBOOK: drawlers due to 0:00 time
;;    org-clock-persist t                     ; Save last clock item after emacs is closed
;;    org-clock-in-resume t                   ; Resume clock if clock in task with not closed clock
;;    org-clock-idle-time 5
;;    org-clock-x11idle-program-name "xprintidle"
;;    org-clock-mode-line-total 'today
;;    org-x11idle-exists-p t                 ; When emacs started as daemon, x11 not initialized
;;    org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 5:00")))
;;    org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
;;    org-clock-history-length 10

;;    ;; ------------------------------------------------------------------------------
;;    ;; Refile
;;    ;; ------------------------------------------------------------------------------
;;    org-refile-use-outline-path 'file           ; Fuzzy match path when refile
;;    org-outline-path-complete-in-steps nil      ; Fuzzy match path when refile

;;    ;; Archive
;;    org-archive-location "~/OneDrive/org/archive/%s_archive::"
;;    org-src-window-setup 'current-window        ; C-c ' is fullscreen

;;    org-publish-project-alist
;;          '(("org-notes"
;;             :base-directory "~/OneDrive/org/"
;;             :publishing-directory "~/public_html/"
;;             :publishing-function org-twbs-publish-to-html
;;             :with-sub-superscript nil

;;             ))

;;    ;; Targets include this file and any file contributing to the agenda - up to 9
;;    ;; levels deep
;;    org-refile-targets `((nil :maxlevel . 9)
;;                               (org-agenda-files :maxlevel . 9)
;;                               ;; ("~/org/dynamic/programming.org" :maxlevel . 1)
;;                               ("~/OneDrive/org/dynamic/todo-someday.org" :level . 1)
;;                               ("~/OneDrive/org/dynamic/notes.org" :level . 0)
;;                               ("~/OneDrive/org/static/diary.org" :maxlevel . 1)
;;                               ("~/OneDrive/org/dynamic/shared-todo.org" :maxlevel . 1)
;;                               ;; ("~/OneDrive/org/static/programming/org" :maxlevel . 2)
;;                               ;; ("~/OneDrive/org/static/programming/programming.org" :maxlevel . 2)
;;                               ;; ("~/OneDrive/org/static/programming/java.org" :maxlevel . 1)
;;                               ,@(mapcar
;;                                (lambda (item) `(,item :maxlevel . 1 ))
;;                                (f-files "~/OneDrive/org/static"
;;                                               (lambda (file) (string= "org" (file-name-extension file))) t))
;;                               )


;;    ;; ------------------------------------------------------------------------------
;;    ;; Edit
;;    ;; ------------------------------------------------------------------------------
;;    ;; I do not want splitting while editing header
;;    org-M-RET-may-split-line '((default . nil) (item . nil))
;;    org-catch-invisible-edits 'show-and-error         ; let's test this option
;;    fill-column 120
;;    ;; ------------------------------------------------------------------------------
;;    ;; Export
;;    ;; ------------------------------------------------------------------------------
;;    org-export-with-toc nil
;;    org-export-with-sub-superscripts nil
;;    org-export-with-section-numbers nil
;;    org-html-validation-link nil
;;    org-enable-bootstrap-support t
;;    ;; ------------------------------------------------------------------------------
;;    ;; Capture
;;    ;; ------------------------------------------------------------------------------
;;    org-capture-templates (my/override-unique-cars
;;                           org-capture-templates
;;                           '(("r" "Remont" item (file+headline "~/OneDrive/org/dynamic/todo.org" "Hotelki")
;;                              "%?")
;;                             ))


;;    ;; ------------------------------------------------------------------------------
;;    ;; Agenda
;;    ;; ------------------------------------------------------------------------------
;;    org-agenda-skip-scheduled-if-done nil    ; Do not shot DONE items
;;    org-agenda-skip-deadline-if-done nil
;;    org-deadline-warning-days 5            ; Warn about deadline

;;    org-agenda-custom-commands (my/override-unique-cars
;;                                org-agenda-custom-commands
;;                                '(("r" "Refile" tags "REFILE"
;;                                                          ((org-agenda-overriding-header "Refile")
;;                                                           (org-tags-match-list-sublevels nil)))

;;                                                         ("B" "Books" todo "INPROGR"
;;                                                          ((org-agenda-overriding-header "Books I am reading")
;;                                                           (org-agenda-sorting-strategy '(user-defined-down))
;;                                                           (org-agenda-cmp-user-defined 'my/org-sort-agenda-logbook)
;;                                                           (org-agenda-files '("~/OneDrive/org/dynamic/todo.org"))))

;;                                                         ("y" "Todo things"
;;                                                          (
;;                                                           ;; + reading
;;                                                           (tags-todo "TODO={INPROGR\\|DOWNLD}+book"
;;                                                                      ((org-agenda-overriding-header "Reading")
;;                                                                       (org-agenda-prefix-format "Reading ")
;;                                                                       ))
;;                                                           ;; - training/practicing
;;                                                           ;; (tags-todo "+emacs"
;;                                                           ;;            ((org-agenda-overriding-header "")
;;                                                           ;;             (org-agenda-prefix-format "Emacs ")))
;;                                                           ;; shopping
;;                                                           (tags-todo "+shopping-errands"
;;                                                                      ((org-agenda-overriding-header "E-shopping")
;;                                                                       (org-agenda-prefix-format "Shopping ")))
;;                                                           ;; household
;;                                                           (tags-todo "+household"
;;                                                                      ((org-agenda-overriding-header "Housekeeping")
;;                                                                       (org-agenda-prefix-format "Household ")))
;;                                                           ;; movie/youtube
;;                                                           (tags-todo "+movie"
;;                                                                      ((org-agenda-overriding-header "Watching movies")
;;                                                                       (org-agenda-prefix-format "Movie ")))
;;                                                           ;; research/googling
;;                                                           (tags-todo "+googling"
;;                                                                      ((org-agenda-overriding-header "Googling")
;;                                                                       (org-agenda-prefix-format "Google ")))
;;                                                           ;; emacs tuning
;;                                                           (tags-todo "emacs"
;;                                                                      ((org-agenda-overriding-header "Emacs tuning")
;;                                                                       (org-agenda-prefix-format "Emacs ")
;;                                                                       (org-agenda-sorting-strategy '(time-up)) ))
;;                                                           ;; system tuning
;;                                                           (tags-todo "linux-learning-emacs"
;;                                                                      ((org-agenda-overriding-header "System tuning")
;;                                                                       (org-agenda-prefix-format "OS ")))
;;                                                           ;; learning
;;                                                           (tags-todo "learning"
;;                                                                      ((org-agenda-overriding-header "Gain skills")
;;                                                                       (org-agenda-prefix-format "Lean ")))
;;                                                           ;; sysadmin chroes
;;                                                           (tags-todo "linux&chore"
;;                                                                      ((org-agenda-overriding-header "OS chores")
;;                                                                       (org-agenda-prefix-format "OS Chore ")))
;;                                                           ;; organizing
;;                                                           (tags-todo "+org"
;;                                                                      ((org-agenda-overriding-header "Organize")
;;                                                                       (org-agenda-prefix-format "Org ")))
;;                                                           ;; uncategorized
;;                                                           (tags-todo "-linux-emacs-learning-org-BOOK-errands"
;;                                                                      ((org-agenda-overriding-header "Rest")
;;                                                                       (org-agenda-prefix-format "Rest ")))
;;                                                           ) (
;;                                                              (org-agenda-sorting-strategy '(time-down))
;;                                                              ))
;;                                                         ;; + shopping
;;                                                         ;; household chores
;;                                                         ;; movie/youtube
;;                                                         ;; research/googling
;;                                                         ;; emacs tuning
;;                                                         ;; system tuning/scripting
;;                                                         ;; learning
;;                                                         ;; sysadmin chores
;;                                                         ;; organizing

;;                                                         (" " "Agenda"
;;                                                          (
;;                                                           (agenda "" nil)
;;                                                           (tags "+lord-lislon+TIMESTAMP_IA>=\"<-7d>\""
;;                                                                 ((org-agenda-overriding-header "Lord/Lislon news (7 days)")
;;                                                                  (org-agenda-prefix-format "     ")
;;                                                                  (org-agenda-sorting-strategy '(timestamp-down))
;;                                                                  (org-agenda-files '("~/OneDrive/lord-lislon/lord-lislon.org")))
;;                                                                 )
;;                                                           )
;;                                                          nil)))


;;    org-tag-alist (my/override-unique-cars
;;                   org-tag-alist
;;                   '(
;;                     ("emacs" . ?e)
;;                    ;; (:startgroup . nil)
;;                    ;; ("torwald". nil)
;;                    ;; ("tatu". nil)
;;                    ;; (:endgroup . nil)
;;                    ))
;;    ;; ------------------------------------------------------------------------------
;;    ;; appt reminders
;;    ;; ------------------------------------------------------------------------------
;;    appt-message-warning-time 60
;;                                         ;appt-display-interval 20

;;    ;; ------------------------------------------------------------------------------
;;    ;; Calendar & Time
;;    ;; ------------------------------------------------------------------------------
;;    calendar-latitude 59.95
;;    calendar-longitude 30.3
;;    calendar-time-zone (* 60 +3)
;;    calendar-standard-time-zone-name "MSK"
;;    calendar-time-display-form '(24-hours ":" minutes
;;                                          (if time-zone " (") time-zone (if time-zone ")"))

;;    ;;------------------------------------------------------------------------------
;;    ;; pomodoro
;;    ;; ------------------------------------------------------------------------------
;;    org-pomodoro-length 20
;;    org-pomodoro-short-break-length 5

;;   ;;;------------------------------------------------------------------------------
;;   ;;; babel - reavaluate code below config to apply changes
;;   ;;; ------------------------------------------------------------------------------
;;    org-confirm-babel-evaluate nil
;;    my-org-babel-load-languages '(
;;                             (shell . t)
;;                             (lisp . t)
;;                             (emacs-lisp . t)
;;                             (java . t)
;;                             (plantuml . t)
;;                             (ditaa . t)
;;                             (dot . t) ;;graphviz
;;                             )
;;    ;;------------------------------------------------------------------------------
;;    ;; UML
;;    ;; ------------------------------------------------------------------------------
;;    org-plantuml-jar-path (cond ((file-exists-p "C:/opt/plantuml/plantuml.jar")  "C:/opt/plantuml/plantuml.jar")
;;                                ((file-exists-p "/opt/plantuml/plantuml.jar")  "/opt/plantuml/plantuml.jar")
;;                                (t nil)
;;                              )
;;    plantuml-jar-path org-plantuml-jar-path   ;; this is used for org-pubsih
;;    ;; ------------------------------------------------------------------------------
;;    ;; TODO faces
;;    ;; ------------------------------------------------------------------------------
;;    org-todo-keyword-faces '(("PENDING" . "SaddleBrown")
;;                             ("DOWNLD" . "LimeGreen")
;;                             ("INPROGR" . "Yellow")
;;                             ("REJT" . "SlateGray")
;;                             ("DONE" . "green2")
;;                             ("APPOINTMENT" . "DarkViolet"))
;;    org-clock-sound "~/OneDrive/confiles/linux/sounds/131348__kaonaya__bell-at-daitokuji-temple-kyoto.wav"
;;    org-show-notification-handler "~/OneDrive/confiles/win/notify-emacs/notify.exe"
;;    )

;;   ;; C-C c capture
;;   ;; (global-set-key "\C-cc" 'org-capture)  ;; Spacemacs by default use C-c c
;;   (global-set-key (kbd "<f1>") 'org-agenda)
;;   ;; Make C-c C-x C-i/o work everywhere (with prefix - clock in recent task)
;;   (global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
;;   (global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
;;   (global-set-key (kbd "C-c C-x C-x") 'org-clock-in-last)
;;   (global-set-key (kbd "C-c C-o") 'org-open-at-point)
;;   (global-set-key (kbd "C-c t") 'my/org-timer-set-timer)
;;   ;; (global-set-key (kbd "<f9>") 'bh/punch-in)
;;   ;; (global-set-key (kbd "<f10>") 'bh/punch-out)
;;   (global-set-key (kbd "<f12>") (defun my/org-clockin-recent-tasks() (interactive)
;;                                        (org-clock-in '(4))))

;;   (autoload 'org-clock-jump-to-current-clock "org-clock")
;;   (global-set-key (kbd "C-c j") 'org-clock-jump-to-current-clock)

;;   ;; (evil-leader/set-key "ga" 'org-agenda)

;;   ;; auto save after 5 seconds
;;   (auto-save-visited-mode)

;;   ;; Insert mode in capture mode
;;   (add-hook 'org-capture-mode-hook 'evil-insert-state)
;;   ;; Insert mode when adding headers
;;   (add-hook 'org-insert-heading-hook 'evil-insert-state)
;;   ;; (add-hook 'org-insert- 'evil-insert-state)

;;   ;; auto clock to parent task
;;   (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;;   ;; Auto fill for org and all text-mode hooks
;;   ;; (remove-hook 'text-mode-hook 'auto-fill-mode)
;;   ;; (add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
;;   (add-hook 'org-timer-done-hook 'my/org-timer-done)

;;   ;; (add-hook 'org-mode-hook 'visual-line-mode)
;;   (add-hook 'org-mode-hook 'visual-fill-column-mode)
;;   (add-hook 'org-mode-hook '(lambda () (setq
;;                                        line-move-visual t
;;                                        fill-column 120
;;                                        )
;;                               (visual-line-mode 1)
;;                               ))


;;   ;; Encryption (ACP NullPointerException on android)

;;   ;; void-function bind-map error:
;;   ;;(evil-leader/set-key-for-mode 'org-mode "of" 'helm-org-in-buffer-headings)

;;   (evil-leader/set-key "bo" (defun my/make-org-buffer() (interactive)
;;                                    (spacemacs/new-empty-buffer)
;;                                    (org-mode)
;;                                    ))
;;   ;; (evil-define-key 'normal evil-org-mode-map "gp" nil)

;;   ;; not using
;;                                         ;(global-set-key (kbd "<f5>") (lambda () (interactive) (org-capture '() "g")))
;;                                         ;(global-set-key (kbd "S-<f5>") (lambda () (interactive) (org-capture '() "G")))
;;                                         ;(global-set-key (kbd "C-<f5>") 'org-clock-jump-to-current-clock)
;;                                         ;(global-set-key (kbd "M-<f5>") (lambda () (interactive) (find-file "~/org/goal-today.org")))

;;   ;; Resume clocking task when emacs is restarted
;;   (with-eval-after-load 'org
;;     (progn
;;       (org-clock-persistence-insinuate)
;;       (org-clock-load)))


;;   ;; Org capture protocol
;;   (with-eval-after-load "org"
;;     (require 'org-protocol)
;;     (add-hook 'org-capture-mode-hook 'my/org-capture-delete-other-windows)
;;     (advice-add 'org-protocol-do-capture :around 'my/intercept-make-capture-frame)
;;     (advice-add 'org-insert-heading :before 'my/org-insert-heading-advice)
;;     )

;;   ;; Crypt facilities
;;   ;; (require 'epa-file)
;;   ;; (epa-file-enable)

;;   (with-eval-after-load "org"
;;     (require 'ob-ditaa)
;;     (require 'ob-plantuml)
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      my-org-babel-load-languages)
;;     ;; Auto insert when ~TAB` in tables
;;     (advice-add 'org-table-next-field :after 'evil-insert-state))

;;   ;; void-function on startup
;;    ;;2k(org-add-link-type "img" 'my-org/org-custom-link-img-follow 'my-org/org-custom-link-img-export)

;;   ;; ------------------------------------------------------------------------------
;;   ;; Update emacs agenda file for awesomewm
;;   ;; ------------------------------------------------------------------------------

;;   ;; update agenda file after changes to org files
;;   ;; (defun th-org-mode-init ()
;;   ;;   (add-hook 'after-save-hook 'th-org-update-agenda-file t t))

;;   ;; (add-hook 'org-mode-hook 'th-org-mode-init)

;;   ;; ;; that's the export function
;;   ;; (defun th-org-update-agenda-file (&optional force)
;;   ;;   (interactive)
;;   ;;   (save-excursion
;;   ;;     (save-window-excursion
;;   ;;       (let ((file "/tmp/org-agenda.txt"))
;;   ;;         (org-agenda-list)
;;   ;;         (org-agenda-write file)))))

;;   ;; ;; do it once at startup
;;   ;; (th-org-update-agenda-file t)

;;   (advice-add 'org-metaright :around #'my/org-metaright-or-evil-shift-right-advice)
;;   (advice-add 'org-metaleft :around #'my/org-metaleft-or-evil-shift-left-advice)
;;   (advice-add 'org-metareturn :around #'my/org-metaleft-or-evil-shift-left-advice)

;;   ;; Auto insert when press C-SPC in tables for clear field
;;   (advice-add 'org-table-blank-field :after (lambda (&rest r) (evil-insert-state)))


;;   ;; Commented for optimization tests
;;   ;; emacs 24.4 No such file or directory
;;   ;; (require 'org-notify)                 ; Support only deadlines
;;   ;; (org-notify-start)
;;   ;; how deadline worked?
;;   (appt-activate 1)

;;   ;; update appt each time agenda opened
;;   (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;;   ;; our little façade-function for djcb-popup
;;   (defun djcb-appt-display (min-to-app new-time msg)
;;     (djcb-popup (format "Appointment in %s minute(s)" min-to-app) msg
;;                 "~/OneDrive/confiles/linux/icons/apppointment-grey-32x32.png"
;;                 "~/OneDrive/confiles/linux/sounds/choir-a.wav"))
;;   (setq appt-disp-window-function 'djcb-appt-display)
;;   (defadvice org-agenda-to-appt (before wickedcool activate)
;;     "Clear the appt-time-msg-list."
;;     (setq appt-time-msg-list nil))

;;   (defhydra my-org-refile-hydra (:color blue :hint nil)
;;     "
;; ^Computers^             ^Lists^           ^Move
;; ^^^^^^------------------------------------------------------
;; _ce_: Emacs          _lB_: book  _ce_: up
;; _cj_: Java           _lb_: buy   _ce_: next visible
;; _cg_: Git            _ce_: -     _ce_: previous visible
;; _cn_: Nice Prog      _ce_: -     _ce_: previous visible
;; _cu_: Unix cmds
;; _cb_: Bash
;; _cw_: Windows cmds
;; "
;;     ("ce" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/computers.org" "Emacs")) "Emacs")
;;     ("cj" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/java.org" "Java")) "Java")
;;     ("cg" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/programming.org" "Git")) "Git")
;;     ("cu" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/computers.org" "How to linux")) "Unix")
;;     ("cw" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/computers.org" "How to windows")) "How to windows")
;;     ("cn" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/computers.org" "Nice Programs")) "Nice Programs")
;;     ("cb" (lambda () (interactive) (my/refile "~/OneDrive/org/programming/bash.org")) "Bash")
;;     ("lB" (lambda () (interactive) (my/refile "~/OneDrive/org/books/books.org" "Books")) "Books")
;;     ("lb" (lambda () (interactive) (my/refile "~/OneDrive/org/personal/buy.org")) "Buy")
;;     ("s" gnus-group-enter-server-mode "Servers")
;;     ("m" gnus-group-new-mail "Compose m OR C-x m")
;;     ("#" gnus-topic-mark-topic "mark #")
;;     ("q" nil "cancel"))

;;   (defhydra my-org-navigator-hydra (:color blue :hint nil)
;;     "
;; ^Work stuff^        ^Shared^
;; ^^^^^^-------------------------
;; _e_: Emacs          _o_: todo
;; _j_: Java           _e_: edit hydra
;; _b_: Bash           _n_: natera
;; _k_: HotKeys        _b_: books
;; _c_: Computers      _p_: programming
;; _l_: Linux          _m_: macos
;; _w_: windows        _J_: JS
;; _E_: English
;; "
;;     ("o" (lambda () (interactive) (find-file "~/OneDrive/org/personal/todo.org")))
;;     ("e" (lambda () (interactive) (find-file "~/OneDrive/dotfiles/spacemacs/layers/my-org/packages.el")))
;;     ("E" (lambda () (interactive) (find-file "~/OneDrive/org/languages/english.org")))
;;     ("n" (lambda () (interactive) (find-file "~/OneDrive/org/work/natera/natera.org")))
;;     ("e" (lambda () (interactive) (my/org-jump-to-file-and-header "~/OneDrive/org/programming/computers.org" "Emacs")))
;;     ("j" (lambda () (interactive) (my/org-jump-to-file-and-header "~/OneDrive/org/programming/java.org" "Java")))
;;     ("J" (lambda () (interactive) (my/org-jump-to-file-and-header "~/OneDrive/org/programming/javascript.org" "Java Script")))
;;     ("b" (lambda () (interactive) (find-file "~/OneDrive/org/programming/bash.org")))
;;     ("p" (lambda () (interactive) (find-file "~/OneDrive/org/programming/programming.org")))
;;     ("c" (lambda () (interactive) (find-file "~/OneDrive/org/programming/computers.org")))
;;     ("w" (lambda () (interactive) (find-file "~/OneDrive/org/programming/windows.org")))
;;     ("l" (lambda () (interactive) (find-file "~/OneDrive/org/programming/linux.org")))
;;     ("k" (lambda () (interactive) (find-file "~/OneDrive/org/programming/hotkeys.org")))
;;     ("m" (lambda () (interactive) (find-file "~/OneDrive/org/programming/macos.org")))
;;     ("g" (lambda () (interactive) (my/org-jump-to-file-and-header "~/OneDrive/org/static/programming/programming.org" "Git")))

;;     ("T" (lambda () (interactive) (find-file "~/OneDrive/org/dynamic/shared-todo.org")))
;;     ("q" nil "cancel"))


;;   (spacemacs/set-leader-keys "oo" 'my-org-navigator-hydra/body)

;;   (spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'my-org-refile-hydra/body)
;;   (global-set-key (kbd "<f5>") 'org-capture)


;;   (advice-add 'switch-to-buffer :before (lambda (&rest misc)
;;                                           (let* ((buffer-save-without-query t))
;;                                             (and (buffer-file-name)
;;                                                  (string-match "org\\'" (buffer-file-name))
;;                                                  (save-buffer)))))


;;   ;; (my/redefine-evilified-key org-agenda-keymap (kbd "v") nil) ;; not works for some reason :(


;;   ;; (defun org-mode-hook-fix-agenda-keys ()
;;   ;;   ;; make v button work in agenda
;;   ;;   (define-key evil-evilified-state-map (kbd "v") nil))

;;   ;; (add-hook 'org-mode-hook 'org-mode-hook-fix-agenda-keys)
;;   (add-hook 'org-open-at-point-functions 'my/org-open-jira-link)
;;   (spacemacs/set-leader-keys "oJ" 'my/org-open-clocking-jira-link)
;;   (spacemacs/set-leader-keys "oj" 'my/org-show-and-copy-jira-ticket)

;;   (with-eval-after-load "org"
;;     (define-key org-mode-map (kbd "C-c C-y") 'my/org-show-and-copy-jira-ticket))

;;   ;; open png externally
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;                (setq org-file-apps
;;                      (append '(
;;                                ("\\.png\\'" . default)
;;                                ) org-file-apps))))

;;   (setq my/global-morning-timer (run-at-time "08:00" (* 24 60 60) 'my/org-morning-clock-chore))
;;   )

;; (defun my-org/init-noflet ())


;; (defun my-org/init-artist ()
;;   (add-hook 'artist-mode-hook
;;             (lambda ()
;;               (evil-emacs-state)
;;               (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
;;               (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
;;               (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
;;               (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
;;               (local-set-key (kbd "C-z") 'undo))
;;   ))

;; (defun my-org/init-helm-yandex-geoapi ()
;;   (use-package helm-yandex-geoapi)
;;   )
