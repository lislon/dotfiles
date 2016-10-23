;; ----------------------------------------------------------------
;; Org mobile sync settings for dedicated emacs instance
;; ----------------------------------------------------------------



(setq-default
 org-mobile-directory "~/Dropbox/MobileOrg"
 org-mobile-files '(append org-agenda-files
                           "~/org/dynamic/refile.org"
                           "~/org/static/books.org"
                           "~/org/dynamic/computers.org"
                           "~/org/archive/books.org_archive"
                           )
 org-mobile-force-id-on-agenda-items nil)



;; ----------------------------------------------------------------
;; Org mobile sync
;; ----------------------------------------------------------------

(defvar my/org-mobile-sync-timer nil)
(defvar my/org-mobile-sync-secs (* 60 30))

(defun my/org-mobile-sync-pull-and-push ()
  (require 'org)
  (condition-case nil
      (progn
        (org-mobile-pull)
        (org-mobile-push))
    (org-notify "Mobile sync error")))

(defun my/org-mobile-start-timer ()
     "Start automated `org-mobile-push'"
     (interactive)
     (setq my/org-mobile-sync-timer
            (run-with-timer my/org-mobile-sync-secs my/org-mobile-sync-secs
            'my/org-mobile-sync-pull-and-push)))

(defun my/org-mobile-sync-stop ()
  "Stop automated `org-mobile-push'"
  (interactive)
  (cancel-timer my/org-mobile-sync-timer))


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

(add-hook 'emacs-startup-hook (lambda ()
  ;; ------------------------------------------------------------------------------
  ;; Settings for dedicated emacs instance, that runs without spacemacs
  ;; ------------------------------------------------------------------------------
  (if (string= server-name "mobile")
      ;; (setq load-path (append load-path (directory-files "~/.emacs.d/elpa" t)))
      (require 'org-clock)

      (setq-default
        ;; ------------------------------------------------------------------------------
        ;; General
        ;; ------------------------------------------------------------------------------
        org-directory "~/org"
        org-agenda-files (quote ("~/org/dynamic")))
    (my/org-mobile-start-timer)
    (my/org-mobile-sync-pull-and-push)
    (message "org-notify")
    (org-notify "Server started")
    )
  ))
