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

(defun my/sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (interactive
   (list
    (completing-read "Choose preset connection: "
                     (mapcar 'car sql-connection-alist))))
  (eval `(let ,(cdr (assoc (intern name) sql-connection-alist))
           (flet ((sql-get-login (&rest what)))
             (sql-product-interactive sql-product sql-user)))))
