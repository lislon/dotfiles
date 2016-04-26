
(defun my/systemd-create-unit (filename)
  "Creates a user systemd file and expands ya-snippet template"
  (interactive "sUnit file name with extension: ")
  (unless (string-match-p "\." filename)
    (setq filename (concat filename ".service")))
  (find-file (concat user-home-directory "/.config/systemd/user/" filename))
  (let* ((extension (file-name-extension filename))
         (templates (yas--all-templates (yas--get-snippet-tables)))
         (template-data  (some (lambda (template)
                                 (and (string= extension (yas--template-name template)) template))
                               templates)))
    (when template-data
      (systemd-mode)
      (yas-minor-mode)
      (evil-insert-state)
      (yas-expand-snippet (yas--template-content template-data)))))

(defun my/spacemacs-buffer//lord-lislon ()
  "Returns lord lislon news"
  (require 'org-agenda)
  )

(defun my/ediff-buffer-with-file (file-B &optional startup-hooks)
  "Run Ediff on a current buffer and other file"
  (interactive
   (list (ediff-read-file-name "File to compare"
                               (setq dir-B
                                     (if ediff-use-last-dir
                                         ediff-last-dir-B
                                       (file-name-directory buffer-file-name)))
                               (progn
                                 (ediff-add-to-history
                                  'file-name-history
                                  (ediff-abbreviate-file-name
                                   (expand-file-name
                                    (file-name-nondirectory buffer-file-name)
                                    dir-B)))
                                 (ediff-get-default-file-name buffer-file-name 1)))
         ))
  (ediff-files-internal (if (file-directory-p file-B)
                            (expand-file-name
                             (file-name-nondirectory file-A) file-B)
                          file-B)
                        buffer-file-name
                        nil ; file-C
                        startup-hooks
                        'ediff-files))

(defun my/sh-extract-variable (varname)
  "Extract WORD under cursor to a variable"
  (interactive "sExtract variable name: ")

  (let* ((inner-word (evil-inner-WORD))
         (beg (car inner-word)) ;; inner-word[0]
         (end (cadr inner-word)) ;; inner-word[1]
         (region (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (insert "$" varname)
    (forward-line -1)
    (newline-and-indent)
    (insert varname "=" region)
    (end-of-line)
    )
  )

(defun my/systemd-enable-unit ()
  (interactive)
  (shell-command (format  "systemctl --user enable %s" (buffer-name))))

(defun my/systemd-disable-unit ()
  (interactive)
  (shell-command (format  "systemctl --user disable %s" (buffer-name))))

(defun my/systemd-start-unit ()
  (interactive)
  (shell-command (format  "systemctl --user start %s" (buffer-name))))

(defun my/systemd-switch-timer-service ()
  "Switch between service/timer files"
  (interactive)
  (let* ((base-name (file-name-sans-extension
                      (buffer-file-name)))
          (service-name (concat base-name ".service"))
          (timer-name (concat base-name ".timer")))
    (cond
      ((and (string= (buffer-file-name) service-name)
            (file-exists-p timer-name))
      (find-file timer-name))
      ((and (string= (buffer-file-name) timer-name)
            (file-exists-p service-name))
      (find-file service-name)))))

(defun my/systemd-move-to-system ()
  "Move systemd files to system"
  (interactive)
  (let* ((base-name (file-name-sans-extension
                      (buffer-file-name)))
          (service-name (concat base-name ".service"))
          (timer-name (concat base-name ".timer")))
    (when (s-starts-with-p "/home")
      ()
      )
    (cond
      ((and (string= (buffer-file-name) service-name)
            (file-exists-p timer-name))
      (find-file timer-name))
      ((and (string= (buffer-file-name) timer-name)
            (file-exists-p service-name))
      (find-file service-name))))
  )

(defun my/spacemacs-maybe-kill-emacs ()
  "If emacs server is running, kills frame instead of server"
  (interactive)
  (if (server-running-p)
      (spacemacs/frame-killer)
    (spacemacs/kill-emacs)))

(defun my/isearch-other-window ()
  "Search in other window"
  (interactive)
  (save-selected-window
    (other-window 1)
    (isearch-forward)))


(defun lsn-insert-line-and-paste (count)
  "Moves to new line and paste text"
  (interactive "P")
  (move-end-of-line nil)
  (newline)
  (evil-paste-after count))

(defun my/keys-help-sheet ()
  "Move to keys cheat sheet"
  (interactive)
  (find-file "~/org/keys.org")
  )

(defun my/google-translate-repl ()
  (interactive)
  (require 'google-translate-default-ui)
  (let ((buffer (get-buffer-create "Google Translate REPL")))
    (switch-to-buffer buffer)
    (google-translate-interactive-mode)
    (evil-insert-state)
    (goto-char (buffer-end 1))
    ))

;; ----------------------------------------------------------------
;; Org mobile sync
;; ----------------------------------------------------------------

(defvar my/org-mobile-sync-timer nil)
(defvar my/org-mobile-sync-secs (* 60 60 24))

(defun my/org-mobile-sync-pull-and-push ()
  (require 'org)
  (org-mobile-pull)
  (org-mobile-push)
  (when (fboundp 'sauron-add-event)
    (sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))

(defun my/org-mobile-sync-start ()
  "Start automated `org-mobile-push'"
  (interactive)
  (setq my/org-mobile-sync-timer
        (run-with-timer my/org-mobile-sync-secs my/org-mobile-sync-secs
                        'my/org-mobile-sync-pull-and-push)))

(defun my/org-mobile-sync-stop ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer my/org-mobile-sync-timer))

(my/org-mobile-sync-start)

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
        (write-region nil nil file)
        )
      )))

(defun bb/erc-github-filter ()
  "Shortens messages from gitter."
  (interactive)
  (when (and (< 18 (- (point-max) (point-min)))
             (string= (buffer-substring (point-min)
                                        (+ (point-min) 18))
                      "<gitter> [Github] "))
    (dolist (regexp '(" \\[Github\\]"
                      " \\(?:in\\|to\\) [^ /]+/[^ /:]+"))
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) t)
        (replace-match "")))
    (goto-char (point-min))
    (when (re-search-forward
           "https?://github\\.com/[^/]+/[^/]+/[^/]+/\\([[:digit:]]+\\)\\([^[:space:]]*\\)?"
           (point-max) t)
      (let* ((url (match-string 0))
             (number (match-string 1))
             (start (+ 1 (match-beginning 0)))
             (end (+ 1 (length number) start)))
        (replace-match (format "(#%s)" (match-string 1)))
        (erc-button-add-button start end 'browse-url nil (list url)))
      )))

(defun eval-parent-sexp ()
  "Cause sometimes you just want to eval just the immediate
form. not the top level, but without going to the closing paren
and evaling there."
  (interactive)
  (save-excursion
    ;; get out of string if in it
    (dotimes (c (if (in-string-p) 2 1))
      (up-list+))
    (let ((cmd (key-binding (kbd "C-x C-e"))))
      (if (eq cmd 'slime-eval-last-expression)
          (funcall cmd)
        (funcall cmd '())))))

;; Do I need this?
(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
This also substitutes \"~\" for the user's home directory (unless the
home directory is a root directory) and removes automounter prefixes
\(see the variable `automount-dir-prefix')."
  ;; Get rid of the prefixes added by the automounter.
  (if (not filename)
      (debug)
      )
  (save-match-data
    (if (and automount-dir-prefix
       (string-match automount-dir-prefix filename)
       (file-exists-p (file-name-directory
           (substring filename (1- (match-end 0))))))
  (setq filename (substring filename (1- (match-end 0)))))
    ;; Avoid treating /home/foo as /home/Foo during `~' substitution.
    ;; To fix this right, we need a `file-name-case-sensitive-p'
    ;; function, but we don't have that yet, so just guess.
    (let ((case-fold-search
     (memq system-type '(ms-dos windows-nt darwin cygwin))))
      ;; If any elt of directory-abbrev-alist matches this name,
      ;; abbreviate accordingly.
      (dolist (dir-abbrev directory-abbrev-alist)
  (if (string-match (car dir-abbrev) filename)
      (setq filename
      (concat (cdr dir-abbrev)
        (substring filename (match-end 0))))))
      ;; Compute and save the abbreviated homedir name.
      ;; We defer computing this until the first time it's needed, to
      ;; give time for directory-abbrev-alist to be set properly.
      ;; We include a slash at the end, to avoid spurious matches
      ;; such as `/usr/foobar' when the home dir is `/usr/foo'.
      (or abbreviated-home-dir
    (setq abbreviated-home-dir
    (let ((abbreviated-home-dir "$foo"))
      (concat "\\`" (abbreviate-file-name (expand-file-name "~"))
        "\\(/\\|\\'\\)"))))

      ;; If FILENAME starts with the abbreviated homedir,
      ;; make it start with `~' instead.
      (if (and (string-match abbreviated-home-dir filename)
         ;; If the home dir is just /, don't change it.
         (not (and (= (match-end 0) 1)
       (= (aref filename 0) ?/)))
         ;; MS-DOS root directories can come with a drive letter;
         ;; Novell Netware allows drive letters beyond `Z:'.
         (not (and (memq system-type '(ms-dos windows-nt cygwin))
       (save-match-data
         (string-match "^[a-zA-`]:/$" filename)))))
    (setq filename
    (concat "~"
      (match-string 1 filename)
      (substring filename (match-end 0)))))
      filename)))

(defun my/compile ()
  "Compile program. With prefix arg change compile args"
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile)
  )

;; Google translate interactive mode
(define-derived-mode google-translate-interactive-mode
  text-mode "Google Translate"
  (defun my/next-line-empty-p ()
    "Check if next line empty"
    (save-excursion
      (beginning-of-line 2)
      (save-match-data
        (looking-at "
[ \t]*$"))
        ))
    (defun my/translate-word-and-next-line ()
      "Shows translation of current line in help buffer and inserts
new line after it"
      (interactive)
      (save-selected-window
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (move-end-of-line nil)
        (google-translate-at-point))
      (if (eq (point) (point-max))
          (newline-and-indent)
        (end-of-line 2)))

    (define-key google-translate-interactive-mode-map (kbd "RET") 'my/translate-word-and-next-line)
    (evil-define-key 'normal google-translate-interactive-mode-map
      (kbd "RET") 'my/translate-word-and-next-line)
)
