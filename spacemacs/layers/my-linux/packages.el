;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-linux-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-linux/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-linux/pre-init-PACKAGE' and/or
;;   `my-linux/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-linux-packages
  '(helm-systemd
    openwith
    org
    dired
    browse-url
    bbdb))


(defun my-linux/post-init-browse-url ()
  (message "my-linux/post-init-browse-url")
  (use-package browse-url
    :config
    ;; Using chrome as default browser

    (message "my-linux/post-init-browse-url 2")
    (when (eq system-type 'gnu/linux)
      ;; On minijack opera is default browser
      ;; lets try browse-url-generic-program, but ensure 'BROWSER' is set
      (if (not (getenv "BROWSER"))
          (setq browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "chromium")
        (setq browse-url-browser-function 'browse-url-xdg-open)))))

(defun my-linux/post-init-dired ()
  ;; Dired - directories first
  (setq  dired-listing-switches "-alkh  --group-directories-first"))

(defun my-linux/post-init-lua-mode ()
  ;; Automode for awesomewm files like rc.lua.blackburg, rc.lua_test
  (add-to-list 'auto-mode-alist '("\\.lua.+" . lua-mode))

  (with-eval-after-load 'flycheck
    (add-hook 'lua-mode-hook flycheck-mode))

  ;; custom error checker for awesomewm just for practice> (`C-c ! s` to select it)
  (with-eval-after-load "lua-mode"
    (with-eval-after-load "flycheck"
      (flycheck-define-checker lua-awesome
                               "A Lua syntax checker using awesome -k."
                               :command ("awesome"
                                         "-k"
                                         "-c" source)
                               :standard-input t
                               :error-patterns
                               ((error line-start
                                       (optional (file-name))
                                       ":" line
                                       ":" (message) line-end))
                               :modes lua-mode)
      ;; enable flycheck for lua (usefull for awesomewm configs)
      ))
  )

(defun my-linux/post-init-java ()
  ;; java multiline comments
  (add-hook 'java-hook (lambda () (local-set-key "\r" 'my-javadoc-return))))

(defun my-linux/post-init-systemd ()
  (add-hook 'systemd-mode-hook (lambda () (setq-local comment-start-skip "#"))))


(defun my-linux/init-ranger ()
  (add-hook 'ranger-mode-hook (lambda () (face-remap-add-relative 'hl-line '((:background "DimGray"))))))

(defun my-linux/init-bbdb ()
  (use-package bbdb))

(defun my-linux/init-helm-systemd ()
  (use-package helm-systemd))

(defun my-linux/init-openwith ()
  (use-package openwith
    :config
    )
  )

(defun my-linux/post-init-org ()
  (setq tea-time-sound-command "aplay")

  )

;;; packages.el ends here
