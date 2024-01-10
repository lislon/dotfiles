(require 'org)
;; ----------------------------------------------------------------
;; Org mobile sync settings for dedicated emacs instance
;; ----------------------------------------------------------------

(setq-default
 org-agenda-files '("~/org/dynamic/"
                    "~/org/dynamic/projects/"
                    "~/OneDrive/workorg/home.org")

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
 org-x11idle-exists-p t                 ; When emacs started as daemon, x11 not initialized
 org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 5:00")))
 org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
 )



;; ----------------------------------------------------------------
;; Org mobile sync
;; ----------------------------------------------------------------


(setq-default
 org-mobile-directory "~/OneDrive/MobileOrg"
 org-mobile-files '("~/org/dynamic/"
                    "~/org/static/books.org"
                    "~/org/archive/books.org_archive"
                    "~/OneDrive/workorg/home.org"
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
(org-mobile-push)
