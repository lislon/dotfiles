(defvar my-common//idea-path "idea" "executable command to open ida")

(with-eval-after-load 'helm
  ;; Helm is barking on start:
  ;; Error running timer ‘require’: (void-variable helm-source-find-files)
  ;; (helm-add-action-to-source-if "IDEA"
  ;;                               'my//helm-open-in-idea
  ;;                               helm-source-find-files
  ;;                               'my//path-to-idea)
  ;; (define-key dired-mode-map (kbd "<f5>") 'my//dired-open-in-idea)
  )

;; fringes (right tiny column with current cursor) for emacs debug
(setq-default fringe-indicator-alist
              '((truncation left-arrow right-arrow)
                (continuation left-curly-arrow right-curly-arrow)
                (overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle
                        bottom-right-angle
                        top-right-angle
                        top-left-angle)
                (top-bottom left-bracket
                            right-bracket
                            top-right-angle
                            top-left-angle)
                (empty-line . empty-line)
                (unknown . question-mark)))

(evil-leader/set-key "ol" 'lsn/calendar-of-life)

(defvar projectile-sticky-project nil "Projectile last sticky project")

(setq magit-repository-directories '("~/src")
      )




;;(define-key dired-mode-map (kbd "<f5>") 'my//dired-open-in-idea)


;; (with-eval-after-load 'helm (progn
;;                               (message "Hi helm")
;;                               (helm-add-action-to-source-if "IDEA"
;;                                                             'my//helm-open-in-idea
;;                                                             helm-source-find-files
;;                                                             'my//path-to-idea))
;;                                )


;; not work on start  :(Error (use-package): helm/:config: No buffer named *helm*)
;;(with-eval-after-load 'helm-files (my-common/idea) )
