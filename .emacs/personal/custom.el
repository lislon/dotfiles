;; Boris
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-M-RET-may-split-line nil)
 '(org-goto-auto-isearch nil)
 '(projectile-indexing-method (quote alien)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Code:
(defun my-find-file-todo (args)
  "Opens my todo.org file."
  (interactive "p")
  (find-file "~/org/todo.org"))

(quail-define-package
 "cyrillic-jcuken" "Cyrillic" "RU" nil
 "ЙЦУКЕH keyboard layout widely used in Russia (ISO 8859-5 encoding)"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8)
 ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=) ("`" ?ё) ("q" ?й) ("w" ?ц) ("e" ?у)
 ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?щ) ("p" ?з) ("[" ?х)
 ("]" ?ъ) ("a" ?ф) ("s" ?ы) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о)
 ("k" ?л) ("l" ?д) (";" ?ж) ("'" ?э) ("\\" ?\\) ("z" ?я) ("x" ?ч) ("c" ?с)
 ("v" ?м) ("b" ?и) ("n" ?т) ("m" ?ь) ("," ?б) ("." ?ю) ("/" ?.) ("!" ?!)
 ("@" ?\") ("#" ?#) ("$" ?\;) ("%" ?%) ("^" ?:) ("&" ??) ("*" ?*) ("(" ?()
 (")" ?)) ("_" ?_) ("+" ?+) ("~" ?Ё)
 ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш)
 ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ъ) ("A" ?Ф) ("S" ?Ы) ("D" ?В) ("F" ?А)
 ("G" ?П) ("H" ?Р) ("J" ?О) ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|" ?/)
 ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?И) ("N" ?Т) ("M" ?Ь) ("<" ?Б)
 (">" ?Ю) ("?" ?,))

(setq default-input-method "cyrillic-jcuken")


;; User initialization goes here
;;(setq default-input-method "cyrillic-yawerty")
;; (evil-leader/set-key "fb" 'open-org-todo)
;; To open C-x r j r

;; (set-register ?l (cons 'file "~/.emacs.d/private/lislon/config.el"))
(setq org-agenda-files (quote ("~/org")))

;; Open file - Caps Lock + j, Caps Lock + f
(keyboard-translate ?\C-j ?\C-x)
(keyboard-translate ?\C-x ?\C-j)

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(setq system-time-locale "C")
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/refile.org"))
(global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cb" 'my-find-file-todo)
(global-set-key "\C-cb" 'org-iswitchb)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)))
      )

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/org/diary.org")

(setq org-startup-indented t)
(setq org-cycle-separator-lines 0)

;; Maximize in start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq org-agenda-ndays 14)

;; C for sunrises
(setq calendar-latitude 59.95)
(setq calendar-longitude 30.30)
(setq calendar-location-name "St. Petersburg, RU")

;;(setq projectile-switch-project-action 'helm-projectile-find-other-file)

(server-start)

(provide 'my)
;;; my.el ends here
