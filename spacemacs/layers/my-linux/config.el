(setq

 ;; java
 eclim-eclipse-dirs "/usr/lib/eclipse"
 eclim-executable "/usr/lib/eclipse/eclim"
 openwith-associations '(
                         ("\\.mp4\\'" "mplayer" (file)))



 ;; gnus
 gnus-save-killed-list nil
 gnus-always-read-dribble-file t
 gnus-init-file "~/Dropbox/confiles/common/emacs/gnus.el"
 ;; gnus-dribble-directory "~/Dropbox/emacs-resources/gnus/"
 gnus-dribble-directory "~/.emacs.d/.cache/gnus"
 gnus-home-directory "~/.emacs.d/.cache/gnus"
 ;; we don't use other news readers
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil

 tea-time-sound-command "aplay %s"
 )

;; q to exit java help
(with-eval-after-load "eclim-java"
  (loop for map in `(,eclim-java-show-documentation-map)
        do
        (define-key map (kbd "q") 'delete-window)
        (evil-define-key 'normal map (kbd "q") 'delete-window)))


;; (when (configuration-layer/package-usedp 'lua)
;;   )

(with-eval-after-load "helm-locate"
  (setq helm-locate-command "locate %s -e %s"))

(with-eval-after-load "sql"
  (sql-set-product-feature 'mysql :prompt-regexp "^\a?MariaDB[^>]+> "))

;; open link in browser (gnus?)
(with-eval-after-load 'w3m
  (define-key gnus-article-mode-map (kbd "RET") 'my/w3m-open-link-or-image-browser)
  (define-key w3m-minor-mode-map (kbd "RET") 'my/w3m-open-link-or-image-browser)
  (define-key w3m-minor-mode-map "F" 'my/w3m-open-link-or-image-browser))

;; Using chrome as default browser
(when (eq system-type 'gnu/linux)
    ;; On minijack opera is default browser
  ;; lets try browse-url-generic-program, but ensure 'BROWSER' is set
  (when (not (getenv "BROWSER"))
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")))
