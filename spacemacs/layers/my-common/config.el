
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

