(setq my-org-packages '(noflet calfw))

(defun my-org/init-noflet ())
(defun my-org/init-calfw ())
(defun my-org/post-init-calfw ()
  (setq cfw:display-calendar-holidays nil)
  (add-hook 'cfw:calendar-mode 'evil-insert))
