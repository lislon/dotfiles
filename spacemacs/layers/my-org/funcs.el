(defun my/org-append-row-to-table (args)
  "Inserts a row at the end of table"
  (interactive "P")
  (if (not (org-at-table-p))
      (user-error "Not at a table"))
  (goto-char (org-table-end))
  (forward-line -1)
  (forward-char)
  (org-table-insert-row 1)
  (evil-insert 1)
  )


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
        (play-sound-file timer-sound))
    (progn (beep t) (beep t)))

(defun my/capture (templates)
  "merge capture-templates with existing org-capture-templates"
  (let* (already-added)
    (remove-if-not (lambda (item)
                     (if (member (car item) already-added)
                         nil
                       (push (car item) already-added)
                       t)) (append templates (if (boundp 'org-capture-templates) org-capture-templates '())))))

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

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(global-set-key (kbd "<f9>") 'bh/punch-in)
(global-set-key (kbd "<f10>") 'bh/punch-out)


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


(defun my/capture-to-current-project ()
  "Insert a capture to the root of currently clocking project"
  (if (and (markerp org-clock-hd-marker)
           (marker-buffer org-clock-hd-marker))
	    (progn (set-buffer (marker-buffer org-clock-hd-marker))
             (org-capture-put-target-region-and-position)
             (widen)
             (bh/find-project-task))
	  (error "No running clock that could be used as capture target")))
