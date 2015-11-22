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
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
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
