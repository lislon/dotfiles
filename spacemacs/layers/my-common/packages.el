;;; packages.el --- my-common layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Igor Avdeev <lislon@mail.ru>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-common-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-common/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-common/pre-init-PACKAGE' and/or
;;   `my-common/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-common-packages
  '(quickrun
    restclient
    vimrc-mode
    ;; undohist -- removed, because not working well with gpg files

    ;; warn: More than one init function found for package all-the-icons. Previous owner was auto-completion, replacing it with layer my-common.
    ;; all-the-icons
    all-the-icons-dired
    sql
    projectile
    elisp-format
    magit
    ;;helm
    hydra
    sh-script
    sh-mode
    dired-mode
    midnight
    ;; vagrant-tramp
)
  "The list of Lisp packages required by the my-common layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;; (defun my-common/init-vagrant-tramp ()
;;   (use-package vagrant-tramp)
;;   )

(defun my-common/init-midnight ()
  "Cleans old emacs buffers"
  (use-package midnight
    :config
    (progn
      (setq clean-buffer-list-delay-general 2 ;; clean buffers older then 2 days
            )
      (midnight-delay-set 'midnight-delay "4:30am")
      )
    )
  )


(defun my-common/post-init-mutiply-cursors ()
  (setq mc/list-file "~/OneDrive/dotfiles/spacemacs/layers/my-common/multiply-allow-commands.el")
  )

(defun my-common/post-init-dired-mode ()
  (message "dired inited")
  (define-key dired-mode-map "e" 'ora-ediff-files)

  ;;(use-package helm
    ;;:defer 1
    ;;:config
    ;;(progn
      ;;(message "helm")
      ;;;; Use F5 to quickly open project in IDEA using <F5> key
      ;;(when (configuration-layer/package-usedp 'helm)
        ;;;; fix Symbol’s value as variable is void: display-time-world-list
        ;;(advice-add #'helm-world-time :before (lambda () (require 'time)))
;;
        ;; Ctrl+Z or F5 in helm
        )
;;
      ;;))
  ;; )

(defun my-common/idea ()
  (interactive)
  (helm-add-action-to-source-if "IDEA"
                                'my//helm-open-in-idea
                                helm-source-find-files
                                'my//path-to-idea)

  (helm-delete-action-from-source "IDEA" helm-source-find-files)
  (define-key dired-mode-map (kbd "<f5>") 'my//dired-open-in-idea)

  )

(defun my-common/post-init-sh-mode ()
  ;; remove execute action if favor og org mode
  (define-key sh-mode-map (kbd "C-c C-x") nil)
  )

(defun my-common/post-init-nxml ()
  (with-eval-after-load 'nxml
    (add-hook 'nxml-mode-hook 'turn-on-evil-matchit-mode)))

(defun my-common/post-init-sql ()

  (evil-leader/set-key
    "am" 'my/sql-select-connection          ; connect to database
    )
  ;; (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)
  (use-package sql
    :defer t
    :config
    (setq sql-input-ring-file-name (expand-file-name "~/.emacs.d/.cache/sql-ring.sql"))
    (add-hook 'comint-preoutput-filter-functions 'my/sql-filter-result))

  (add-hook 'sql-interactive-mode-hook 'spacemacs/toggle-truncate-lines-on)

  (advice-add #'sql-execute-feature :after 'my/sql-list-tables-advice)

  ;; (require 'fakecygpty)
  ;; (fakecygpty-activate)
  )



(defun my-common/init-restclient ()
  (evil-leader/set-key "aR" 'my/rest-client)
  (use-package restclient
    :defer t))

(defun my-common/init-pallete ()
  (use-package pallete
    :defer t
    ))

(defun my-common/init-vimrc-mode ()
  (use-package vimrc-mode
    :defer t))


(defun my-common/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

;; (defun my-common/init-all-the-icons ()
;;   (use-package all-the-icons))

(defun my-common/post-init-dired ()
  (use-package dired
    :bind (:map dired-mode-map
                ("e" . wdired-change-to-wdired-mode)))
  )


;;(defun my-common/post-init-helm ()
;;   (setq
;;    helm-locate-command "locate %s -e %s"
;;    ;; I enabled this on for better candidate matching like recentf gnus.el
;;    ;; helm-ff-auto-update-initial-value t
;;    )
;;   (global-set-key (kbd "M-a") 'helm-mini)
;;   ;; recent-f sort minor mode
;;   (helm-adaptive-mode)
;;   (evil-leader/set-key "hl" 'helm-locate-library)
;;   (eval-after-load "helm-files"
;;     '(setq helm-source-recentf
;;            (helm-make-source "Recentf" 'helm-recentf-source
;;              :filtered-candidate-transformer nil
;;              )))

;;   ;; Use F5 to quickly open project in IDEA using <F5> key
;;   (when (configuration-layer/package-usedp 'helm)
;;     ;; fix Symbol’s value as variable is void: display-time-world-list
;;     (advice-add #'helm-world-time :before (lambda () (require 'time)))

;;     ;; Ctrl+Z or F5 in helm
    ;;(helm-add-action-to-source-if "IDEA"
                                  ;;'my//helm-open-in-idea
                                  ;;helm-source-find-files
                                  ;;'my//path-to-idea)

;;     ;; (helm-delete-action-from-source "IDEA" helm-source-find-files)
;;     )

;;   (define-key dired-mode-map (kbd "<f5>") 'my//dired-open-in-idea)
  ;;)


(defun my-common/post-init-quickrun ()
  ;; (use-package quickrun)
  (advice-add 'quickrun :before (lambda (&rest PLIST) (save-buffer)))
  (quickrun-add-command "TypeScript-tsx"
    (cdr (assoc "typescript"  quickrun--language-alist)))
  )

;; (defun my-common/init-undohist ()
;;   (use-package undohist
;;     :config (undohist-initialize))
;;   )

(defun my-common/post-init-yasnippet ()
  ;; documentation for snippet mode
  (spacemacs/set-leader-keys-for-major-mode
    'snippet-mode "h" (lambda () (interactive)
                        (browse-url "http://joaotavora.github.io/yasnippet/snippet-development.html"))))

(defun my-common/post-init-projectile ()
  (advice-add #'projectile-project-root :before-until #'my/projectile-use-sticky-project-advice)
  (advice-add #'helm-projectile-switch-project :around 'my/projectile-helm-advice)
  (evil-leader/set-key
    "ps" 'my/projectile-magit-status          ; connect to database
    ))

(defun my-common/init-elisp-format ()
  (use-package elisp-format
    :defer t)
  )

(defun my-common/post-init-magit ()
  (with-eval-after-load 'magit

    ;; same code but complex...
    ;; (cl-loop for map-name in '(magit-diff-mode-map magit-status-mode-map)
    ;;          do (cl-loop for x from 1 to 4 do
    ;;                      (define-key (symbol-value map-name)
    ;;                        (kbd (format "M-%d" x))
    ;;                        (make-symbol (format "winum-select-window-%d" x)))))

      (define-key magit-diff-mode-map (kbd "M-1") 'winum-select-window-1)
      (define-key magit-diff-mode-map (kbd "M-2") 'winum-select-window-2)
      (define-key magit-diff-mode-map (kbd "M-3") 'winum-select-window-3)
      (define-key magit-diff-mode-map (kbd "M-4") 'winum-select-window-4)

      (define-key magit-status-mode-map (kbd "M-1") 'winum-select-window-1)
      (define-key magit-status-mode-map (kbd "M-2") 'winum-select-window-2)
      (define-key magit-status-mode-map (kbd "M-3") 'winum-select-window-3)
      (define-key magit-status-mode-map (kbd "M-4") 'winum-select-window-4)
  ))


(defun my-common/post-init-hydra ()
  (defhydra my-config-nav-hydra (:color blue :hint nil)
    "
               ^Layers^
^^^^^^------------------------------------
_c_: Common       _o_: Org       _w_: Work

"
    ("w" (lambda () (interactive) (find-file "~/.spacemacs.d.local/layers/my-work/packages.el")))
    ("o" (lambda () (interactive) (find-file "~/.spacemacs.d/layers/my-org/packages.el")))
    ("c" (lambda () (interactive) (find-file "~/.spacemacs.d/layers/my-common/packages.el")))
    ("q" nil "cancel"))
  (spacemacs/set-leader-keys "oc" 'my-config-nav-hydra/body)
)


(defun my-common/post-init-sh-script ()
  (message "post init sh script")
  (with-eval-after-load 'sh-script
    (define-key sh-mode-map (kbd "C-c C-x") nil)))
;; Do not overwrite history on buffer close
