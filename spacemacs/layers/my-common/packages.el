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
    palette
    vimrc-mode
    undohist
    yasnippet
    all-the-icons
    all-the-icons-dired
    sql
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

(defun my-common/post-init-nxml ()
  (add-hook 'nxml-mode-hook 'turn-on-evil-matchit-mode))

(defun my-common/post-init-sql ()
  "docstring"

  ;; (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)
  (use-package sql
    :config
    (setq sql-input-ring-file-name (expand-file-name "~/.emacs.d/.cache/sql-ring.sql"))
    (add-hook 'comint-preoutput-filter-functions (lambda (text)
                                                   (with-temp-buffer
                                                     (setq-local tab-width 8)
                                                     (insert text)
                                                     (untabify (point-min) (point-max))
                                                     (buffer-string)

                                                     ;; dirty hack to save kill ring each time string sent to server
                                                     ;; otherwise history lost when emacs crahsed or i delete buffer by hand
                                                     (let
                                                         ((comint-input-ring-separator sql-input-ring-separator)
                                                          (comint-input-ring-file-name sql-input-ring-file-name))
                                                       (comint-write-input-ring))
                                                     )

                                                   ))

    )

  ;; (require 'fakecygpty)
  ;; (fakecygpty-activate)
  )

(defun my-common/init-restclient ()
  (use-package restclient))

(defun my-common/init-pallete ()
  (use-package pallete))

(defun my-common/init-vimrc-mode ()
  (use-package vimrc-mode))


(defun my-common/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))

(defun my-common/init-all-the-icons ()
  (use-package all-the-icons))

;;; packages.el ends here
(defun my-common/post-init-sql ()
  ;; no wrap lines
  (add-hook 'sql-interactive-mode-hook 'spacemacs/toggle-truncate-lines-on)
  )

(defun my-common/post-init-yasnippet ()
  (use-package yasnippet
    :config
    (setq yas-snippet-dirs
    (cons
     (expand-file-name "~/.spacemacs.d/snippets")
     (cdr yas-snippet-dirs)))))

(defun my-common/post-init-dired ()
  (use-package dired
    :bind (:map dired-mode-map
                ("e" . wdired-change-to-wdired-mode)))
  )

(defun my-common/post-init-helm ()
  (setq
   helm-locate-command "locate %s -e %s"
   ;; I enabled this on for better candidate matching like recentf gnus.el
   ;; helm-ff-auto-update-initial-value t
   )
  (global-set-key (kbd "M-a") 'helm-mini)
  ;; recent-f sort minor mode
  (helm-adaptive-mode)
  (evil-leader/set-key "hl" 'helm-locate-library))

(defun my-common/post-init-quickrun ()
    (advice-add 'quickrun :before (lambda (&rest PLIST) (save-buffer))))

;; (defun my-common/init-undohist ()
;;   (use-package undohist
;;     :config (undohist-initialize))
;;   )

(defun my-common/init-palette ()
  (use-package palette))

(defun my-common/post-init-helm ()
  (eval-after-load "helm-files"
    '(setq helm-source-recentf
           (helm-make-source "Recentf" 'helm-recentf-source
             :filtered-candidate-transformer nil
             ))))

(defun my-common/post-init-yasnippet ()
  ;; documentation for snippet mode
  (spacemacs/set-leader-keys-for-major-mode
    'snippet-mode "h" (lambda () (interactive)
                        (browse-url "http://joaotavora.github.io/yasnippet/snippet-development.html"))))

;; Do not overwrite history on buffer close
