(defun my/org-append-row-to-table (args)
  "Inserts a row at the end of table"
  (interactive "P")
  (if (not (org-at-table-p))
      (user-error "Not at a table"))
  (goto-char (org-table-end))
  (forward-line -1)
  (forward-char)
  (org-table-insert-row 1)
  (evil-insert 1))


(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-entry-get nil "TIMESTAMP_IA" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (save-excursion
      (save-current-buffer
        (org-cut-subtree)
        (if file (set-buffer (find-file-noselect file)))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (ignore-errors
          (org-paste-subtree 4))
        (widen)
        ))
    )
  )

;; ------------------------------------------------------------------------------
;; Make small capture window for org-protocol
;; ------------------------------------------------------------------------------
(defun my/intercept-make-capture-frame (func &rest r)
  "Makes small org-capture window when invoking org-protocol"
  (require 'noflet)
  (helm-mode -1)
  (hidden-mode-line-mode)
  (noflet (
           (switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (condition-case nil
        (apply func r)
      (error (progn
               (helm-mode 1)
               (spacemacs/frame-killer)))))
  (helm-mode 1)
  )

(defun my/org-insert-heading-advice (&optional arg invisible-ok top-level)
  "Always insert new header below on M-RET"
  (if (and (org-at-heading-p) (eq (line-beginning-position) (point)))
      (goto-char (+ 1 (point))))
  ;; insert mode when inserting new list item from normal mo
  (if (org-at-item-p)
      (evil-insert-state)))


(defun my/org-capture-delete-other-windows (&optional args)
  (interactive "P")
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")
(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

(defun my/org-sort-agenda-logbook (a b)
  "Compares by last clocked out task"
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (fa (and ma (marker-buffer ma)))
         (fb (and mb (marker-buffer mb)))
         (ta (and fa (with-current-buffer fa (goto-char ma)(org-clock-get-last-clock-out-time))))
         (tb (and fb (with-current-buffer fb (goto-char mb)(org-clock-get-last-clock-out-time))))
         )
    (cond ((equal ta tb) nil)
           ((and ta tb (time-less-p ta tb)) -1)
          (t +1))))

(defun my/lord-lislon-auto-insert-date-heading ()
  "Automatically adds lislon or lord tags and timestamp when adding new headings"
  (let ((cur (point)))
    (insert (concat " :" my/lord-lislon-tag ":"))
    (newline-and-indent)
    (org-time-stamp '(16) 'inactive)
    (goto-char cur))
  )

(defun djcb-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    ;; text only version

    (message (concat title ": " msg)))
  (when sound (org-clock-play-sound sound)))

(defun my/org-goal-today-after-done ()
  "Move DONE-ed item to end of file for goal-today.org"
  (interactive)
  (when (member (org-get-todo-state) (list "DONE"))
    (org-cut-subtree)
    (goto-char (point-max))
    (org-paste-subtree))
  )

(defun my/org-goal-today-finish-day ()
  (interactive)
  (goto-char (point-min))
  (org-forward-heading-same-level 1)
  (beginning-of-line)
  (let ((beg (point)) end)
    (end-of-line)
    (setq end (point))

    (let ((region (buffer-substring-no-properties beg end) ))
      (org-refile-to-datetree "~/org/diary.org")
      (insert "* Task stack")
      (newline-and-indent)
      (org-time-stamp-inactive '(16))
      )
    ))

(defun my/org-metaright-or-evil-shift-right-advice (orig-func &rest args)
  "Overrides org-metaright if cursor is not at heading or item"
  (message "at-heading-or-item: %s" (org-at-heading-or-item-p))
  ;; (message "at-block: %s" (org-at-block-p))
  ;; (message "at-item-desc: %s" (org-at-item-description-p))
  ;; (apply orig-func args)
  (if (org-at-heading-or-item-p)
      (apply orig-func args)
    (call-interactively 'evil-shift-right))
  )
(defun my/org-metaleft-or-evil-shift-left-advice (orig-func &rest args)
  "Overrides org-metaleft if cursor is not at heading or item"
  (message "at-heading-or-item: %s" (org-at-heading-or-item-p))

  (if (org-at-heading-or-item-p)
      (apply orig-func args)
    (call-interactively 'evil-shift-left))
  )

;; Autosave all org & config file buffers for Dropbox
(defun auto-save-all-files ()
  (interactive)
  (save-excursion
    (unless (evil-insert-state-p)
      (dolist (buf (buffer-list))
        (set-buffer buf)
        ;; (message "Autosave: Buffer %s modified? %s" (buffer-file-name) (buffer-modified-p))
        (if (and (buffer-file-name) (buffer-modified-p))
            (if (or (eq major-mode 'emacs-lisp-mode) (eq major-mode 'org-mode))
                ;; (message "Autosave: yes!")
                (basic-save-buffer))
          )))))

(defun my/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    )))

(defun my/org-timer-done ()
  "Functions called when timer is done"
  (if (and (boundp 'timer-sound) (boundp 'tea-time-sound-command))
          (start-process-shell-command "tea-ready" nil (format tea-time-sound-command timer-sound))
    (play-sound-file (expand-file-name timer-sound)))
    (progn (beep t) (beep t)))

(defun my/uniq-car (templates)
  "Merge capture-templates with existing org-capture-templates"
  (let* (already-added
         (old-templates (if (boundp 'org-capture-templates) org-capture-templates '()))
         (merged-templates (append templates old-templates))
         )
    (remove-if (lambda (item)
                     (if (member (car item) already-added)
                         ;; item was not existed before - remove it
                         t
                       ;; otherwise add to already-added
                       (push (car item) already-added)
                       nil))
              merged-templates)))

(defun my/archive-subtree (file heading &optional days)
  "Archives all entries under given `heading' in `file', older then `days' days"
  (let* ((buffer (get-file-buffer file))
         (marker (org-find-exact-headline-in-buffer heading buffer))
         (days-purge (if days days 10))
         (counter 0))
    (save-excursion
      (save-restriction
        (set-buffer buffer)
        (goto-char marker)
        (org-goto-first-child)
        (while (org-on-heading-p)
          (when (> (org-time-stamp-to-now (org-entry-get nil "TIMESTAMP_IA")) days-purge)
            (setq counter (+1 counter))
            (org-archive-subtree))
          (outline-next-visible-heading 1))
        (message "%s entries archived at heading %s" counter heading)))))



;;(my/archive-subtree "/home/ele/shared-org/dynamic/tasks.org" "Appointments")


(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))


(defun bh/clock-out-maybe ()
  (when (and (not org-clock-clocking-in)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(defun my/capture-to-current-project ()
  "Insert a capture to the root of currently clocking project"
  (if (and (markerp org-clock-hd-marker)
           (marker-buffer org-clock-hd-marker))
	    (progn (set-buffer (marker-buffer org-clock-hd-marker))
             (org-capture-put-target-region-and-position)
             (widen)
             (bh/find-project-task))
	  (error "No running clock that could be used as capture target")))

(defmacro my/override-unique-cars (var-name new-list)
  "Merges an existing variable `var-name' with new list `new-list'.
The resulting list is uniqied by car element's

Used to override org-captures values"
  `(let* (already-added
          (old-templates (if (boundp ',var-name) ,var-name '()))
          (merged-templates (append ,new-list old-templates)))
     (remove-if (lambda (item)
                  (if (member (car item) already-added)
                      ;; item was not existed before - remove it
                      t
                    ;; otherwise add to already-added
                    (push (car item) already-added)
                    nil))
                merged-templates)))

(defun my-advice-org-atpoint-jira (func &rest args)
  "Open a JIRA property link when C-c C-o on TODO project entry"
  (if (my/org--at-jira-entry-p)
      (browse-url (my/org--jira-get-url-at-point ))
    (apply func args)))

(defun my/org--at-jira-entry-p ()
  "Returns true if point is on header with jira property"
  (and (org-at-heading-p) (my/org--jira-get-url-at-point )))

(defun my/org--jira-get-url-at-point ()
  "Returns a url to JIRA task under point"
  (org-entry-get (point) "JIRA"))

(defun my/org-open-jira-link ()
  "Open a JIRA property link when C-c C-o on TODO project entry"
  (when (my/org--at-jira-entry-p)
    (browse-url (my/org--jira-get-url-at-point))
    t))

(defun my/org-open-clocking-jira-link ()
  "Opens a jira link of currently clocking item"
  (interactive)
  (save-window-excursion
    (save-restriction
      (org-clock-goto)
      (bh/find-project-task)
      (my/org-open-jira-link)
      )))

(defun my/org-jump-to-file-and-header (file &optional headline)
  "Opens a file `file' and jumps to headline `headline'"
  (and (find-file file)
       (if headline
           (org-find-exact-headline-in-buffer headline)
         (move-marker (make-marker) (match-beginning 0))
         )))

(defun my/refile (file &optional headline)
  "Refile current entry to file+headline"
  (save-window-excursion
    (save-restriction
      (let* ((pos (save-excursion
                    (my/org-jump-to-file-and-header file headline))))
        (if pos
            (org-refile nil nil (list headline file nil pos))
          (error (format "Headline '%s' not found in file %s" headline file))
          )))
    ))


(defmacro my/hydra-refile(key file headline)
    "Returns a list used by hydra to refile headline at cursor to `file' under `headline'"
  `(list ,key (list 'lambda (list) (list 'interactive) (list 'my-refile ,file ,headline) ) ,headline))

(defun my/org-show-and-copy-jira-ticket ()
  "Show and copy jira ticket under currenly selected task to ring buffer"
  (interactive)
  (save-window-excursion
    (save-restriction
      (org-clock-goto)
      (bh/find-project-task)
      (when (my/org--at-jira-entry-p)
        (let* ((url (my/org--jira-get-url-at-point))
               (ticket (car (last (split-string url "/")))))
          (kill-new ticket)
          (message ticket))))))

(defun my/org-clean-up-after-my-master ()
  "Clock out from last task"
  (when (org-clocking-p)
    (org-clock-out)))

(defun my/org-morning-clock-chore ()
  "Resets the `org-clock-out-time' at morning when I come at work"
  (my/clean-up-after-my-master-at-morning)
  (my/org-clean-up-after-my-master)
  )

(defun my/org-wait-for-my-master-at-morning ()
  "Resets the `org-clock-out-time' at morning when I come at work"
  (setq my/org-wait-for-master-timer
        (run-with-timer 60 60 'my/org-when-my-master-return-reset-clock)))



(defun my/org-when-my-master-return-reset-clock ()
  (let* ((org-clock-user-idle-seconds (org-user-idle-seconds)))
    (message "Morning Now is %s .Idle time: %s" (current-time-string) org-clock-user-idle-seconds)
    (when (< org-clock-user-idle-seconds 60)
      (message "reset clock time for you")
      (setq org-clock-out-time (org-current-time org-clock-rounding-minutes))
      (cancel-timer my/org-wait-for-master-timer)
    )))

(defun my/org-search ()
  "Seachs in org directoy"
  (interactive)
  (spacemacs/helm-files-do-rg "~/org")
  )
