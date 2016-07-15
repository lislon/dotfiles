
(defun my/make-temp-java-sandbox (project-name)
  "Generates a temporarty java file with boilerplate hello world example"
  (interactive "sName of project: ")
  (let ((dir (make-temp-file (concat "sandbox-java-" project-name) t)))
    (find-file (concat (file-name-as-directory dir) (concat (capitalize project-name) ".java")))
    (eclim-mode nil)
    (my/insert-yasnippet "sandbox" (list 'class-name (capitalize project-name)))
    ;; (quickrun-autorun-mode)
    ))
(defun my/save-java-sandbox (project-name)
  "Exports java sandbox environment to persistent location"
  (interactive "sName of project in ~/src: ")
  (message project-name)
  (when (string-match-p "sandbox-java" buffer-file-name)
    (let* ((temp-proj-dir (file-name-directory buffer-file-name))
          (new-proj-dir (concat (file-name-directory
                                 my/java-sandbox-persist-dir) project-name))
          (bufs (cl-loop for buf in (buffer-list)
                         if (and (buffer-file-name buf)
                                 (string-prefix-p temp-proj-dir (buffer-file-name buf)))
                         collect buf)))
      (mapc 'save-buffer bufs)
      (copy-directory temp-proj-dir new-proj-dir t t)
      (delete-directory temp-proj-dir t)
      (find-file (concat (file-name-directory new-proj-dir) (file-name-nondirectory buffer-file-name)) )
      (mapc 'kill-buffer bufs)
      (message "Project was exported to %s" new-proj-dir)
      )))
(defcustom my/java-sandbox-persist-dir "~/src/" "Directory used
for export java sandbox projects")

(defun my/insert-yasnippet (name &rest expand-env)
  "Inserts snippet with given name into current buffer

EXPAND-ENV is a list of (SYM VALUE) let-style dynamic bindings
considered when expanding the snippet.
"
  (let* ((templates (yas--all-templates (yas--get-snippet-tables)))
         (template-data  (some (lambda (template)
                                 (and (string= name (yas--template-name template)) template))
                               templates)))
    (when template-data
      (evil-insert-state)
      (yas-expand-snippet (yas--template-content template-data) nil nil expand-env))))

(defun my/systemd-create-unit (filename)
  "Creates a user systemd file and expands ya-snippet template"
  (interactive "sUnit file name WITH (!) extension: ")
  (unless (string-match-p "\." filename)
    (setq filename (concat filename ".service")))
  (find-file (concat user-home-directory "/.config/systemd/user/" filename))
  (my/insert-yasnippet (file-name-extension filename))
  )

(defun my/spacemacs-buffer//lord-lislon ()
  "Returns lord lislon news"
  (require 'org-agenda)
  )

(defun my/ediff-buffer-with-file (file-B &optional startup-hooks)
  "Run Ediff on a current buffer and other file. Need to require ediff somewhere"
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
    (evil-backward-paragraph)
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
  ;; Check local buffer variable if we editing files from console.
  (if server-buffer-clients
      ;; if yes, just kill buffer it to return to console
      (kill-buffer)
    ;; Otherwise kill frame
    (if (server-running-p)
        (spacemacs/frame-killer)
      (spacemacs/kill-emacs))))

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
    (define-key google-translate-interactive-mode-map (kbd "RET") 'my/translate-word-and-next-line)
    (evil-define-key 'normal google-translate-interactive-mode-map (kbd "RET") 'my/translate-word-and-next-line)
    (add-hook 'google-translate-interactive-mode-hook
              (lambda ()
                (add-hook 'kill-buffer-hook
                          'my/google-translate-append-to-dictionary t t)))
)

(defun my/google-translate-append-to-dictionary ()
  "Appends words from interactive buffer to dictionary"
  (when my/english-dictionary-file
    (append-to-file (point-min) (point-max) my/english-dictionary-file)))

(defvar my/english-dictionary-file nil "Filename of txt file,
that is storing words from interactive google translate buffer")

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

(defun my/next-line-empty-p ()
  "Check if next line empty"
  (save-excursion
    (beginning-of-line 2)
    (save-match-data
      (looking-at "
[ \t]*$"))))

(defun my/init-swiper ()
  (use-package swiper
    :config
    (progn
      (defun spacemacs/swiper-region-or-symbol ()
        "Run `swiper' with the selected region or the symbol
around point as the initial input."
        (interactive)
        (let ((input (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning) (region-end))
                       (thing-at-point 'symbol t))))
          (swiper input)))

      (defun spacemacs/swiper-all-region-or-symbol ()
        "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
        (interactive)
        (ivy-read "Swiper: " (swiper--multi-candidates
                              (cl-remove-if-not
                               #'buffer-file-name
                               (buffer-list)))
                  :initial-input (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (thing-at-point 'symbol t))
                  :action 'swiper-multi-action-2
                  :unwind #'swiper--cleanup
                  :caller 'swiper-multi))

      (spacemacs/set-leader-keys
        "ss" 'swiper
        "sS" 'spacemacs/swiper-region-or-symbol
        "sb" 'swiper-all
        "sB" 'spacemacs/swiper-all-region-or-symbol)
      (global-set-key "\C-s" 'swiper)
      )))

(defun eww/init-eww ()
  (use-package eww
    :defer t
    :config
    (evilified-state-evilify-map eww-mode-map
      :bindings
      (kbd "H") 'eww-back-url
      (kbd "L") 'eww-next-url
      (kbd "B") 'eww-bookmark-browse
      )
    ))

(defun my-javadoc-return ()
  "Advanced C-m for Javadoc multiline comments.
Inserts `*' at the beggining of the new line if
unless return was pressed outside the comment"
  (interactive)
  (setq last (point))
  (setq is-inside
        (if (search-backward "*/" nil t)
            ;; there are some comment endings - search forward
            (search-forward "/*" last t)
          ;; it's the only comment - search backward
          (goto-char last)
          (search-backward "/*" nil t)
          )
        )
  ;; go to last char position
  (goto-char last)
  ;; the point is inside some comment, insert `* '
  (if is-inside
      (progn
        (insert "\n* ")
        (indent-for-tab-command))
    ;; else insert only new-line
    (insert "\n")))

(defun my/java-text ()
  "Search phrase from java book"
  (interactive)
  (save-buffer)
  (save-excursion
    (save-restriction

      (spacemacs//helm-do-grep-region-or-symbol (list  "~/Dropbox/emacs-resources/abc-java-perfomance.txt")))))
