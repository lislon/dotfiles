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

