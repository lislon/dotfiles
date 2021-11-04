;; -*- lexical-binding: t -*-



(defun lsn/calendar-of-life ()
  (interactive)
  "Shows how many weeks left in my life"
  (let* ((weeks-in-year 52)
         (total-years 65)
         (birthday (date-to-time "1988-07-01 00:00:00 +0300"))
         (counter 0)
         (weeks-i-live (/ (time-to-number-of-days (time-since birthday)) 7))
         (buffer (get-buffer-create "Calendar of life")))
    (switch-to-buffer buffer)
    (erase-buffer)
    (while (< counter (* total-years weeks-in-year))
      (when (eq 0 (% counter weeks-in-year))
        (when (> counter 0)
          (insert "\n"))
        (insert (format "%2d" (+ 0 (/ counter weeks-in-year)))))
      (if (< counter weeks-i-live)
          (insert " x")
        (insert " -"))
      (setq counter (1+ counter))
      )
    ))

(defun ora-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
(defun my/sql-filter-result (text)
  "Tabify sql result to show nicely aligned tables"
  (let ((comint-input-ring-separator sql-input-ring-separator)
        (comint-input-ring-file-name sql-input-ring-file-name))
    (comint-write-input-ring))
  (with-temp-buffer
    (setq-local tab-width 8)
    (insert text)
    (untabify (point-min) (point-max))
    (buffer-string)
    ;; dirty hack to save kill ring each time string sent to server
    ;; otherwise history lost when emacs crahsed or i delete buffer by hand
    ))

(defun my/sql-list-tables-advice (sqlbuf name &rest rest)
  "Untabify buffer, which is is used to show table details"
  (message "my/sql-list-tables-advice")
  (message sqlbuf)
  (message name)
  (when (string-match "\*List " name)
    (with-current-buffer
      (setq-local tab-width 8)
      (untabify (point-min) (point-max))
      )))

(defun my/sql-select-connection ()
  "Used for advice before my/sql-connect-preset"
  (interactive)
  (call-interactively 'my/sql-connect-preset))

(defun my/sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (interactive
   (list
    (completing-read "Choose preset connection: "
                     (mapcar 'car sql-connection-alist))))
  (eval `(let ,(cdr (assoc (intern name) sql-connection-alist))
           (flet ((sql-get-login (&rest what)))
             (sql-product-interactive sql-product sql-user)))))

(defun my/projectile-use-sticky-project-advice (&rest args)
  "Skip calling `projectile-project-root' when there is a main project defined."
  (when projectile-sticky-project
    projectile-sticky-project))



(defun my/projectile-helm-advice (orig-fun &rest args)
  "Temporary clears the `current-project' variable to allow projectile-helm to select new sticky project"
  (setq projectile-sticky-project nil)
  (let ((current-project))
    (let ((projectile-sticky-project nil))
      (apply orig-fun args)
      (setq current-project (projectile-project-root)))
    (setq projectile-sticky-project current-project)))


(defun my/projectile-magit-status ()
  "Opens magit status for currenly selected projectile directory"
  (interactive)
  (magit-status projectile-sticky-project))

(defun my/local-key-interactive (key func)
  "Sets the key binding for current buffer only"
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
    (funcall (intern name) t)))


(defun my//helm-can-be-opened-in-idea (cand)
  "Is helm canddidate can be opened in IDEA"

    (condition-case nil
        (progn
          (cond
           ((string-suffix-p ".iml" cand) t)
           ((and
             (directory-name-p cand)
             (or
              (file-exists-p (concat cand ".idea"))
              (file-exists-p (concat cand "src"))
              )
             )) t
           t t
           )
          )
      (error nil))
  )

(defun my//path-to-idea (path)
  "Return first found path to .idea project from current directory"
  (let
      ((dir (if (directory-name-p path) path (directory-file-name path))))
    (if (or
         (file-exists-p (concat dir ".idea"))
         (file-exists-p (concat dir "pom.xml"))
         (and (file-exists-p (concat dir "src"))
              (file-expand-wildcards (concat dir "src/*.java" ) )))

        (progn
          (message "yes! :%s " (concat dir "src/*.java" ))
          (directory-file-name dir))
      (let* ((parent-dir (file-name-directory (directory-file-name dir))))
        (if (not (string-equal dir parent-dir))
            (my//path-to-idea parent-dir)
          nil
          )))))


(defun my//helm-open-in-idea (_candidate)
  "Opens selected directory/file in IDEA"
  (let* (
         (cand (car (helm-marked-candidates :with-wildcard t)))
         (path (my//path-to-idea cand)))
    (message "IDEA: %s" path)
    (my//run-idea path)))

(defun my//run-idea (path)
   (if (eq system-type 'windows-nt)
    (eshell-command-result (encode-coding-string (format "idea64 \"%s\"" path) 'windows-1251))
    (eshell-command-result (format "idea \"%s\"" path) ))
  )

(defun my//dired-open-in-idea ()
  "Opens selected directory/file in IDEA"
  (interactive)
  (let* ((path (my//path-to-idea (dired-file-name-at-point))))
    (message "IDEA: %s" path)
    (my//run-idea path)))
