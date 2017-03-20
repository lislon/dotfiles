;;; packages.el --- my-windows layer packages file for Spacemacs.
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
;; added to `my-windows-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-windows/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-windows/pre-init-PACKAGE' and/or
;;   `my-windows/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-windows-packages
  '((gntp :toggle (file-exists-p "growlnotify.exe"))
    (alert :location (recipe
                      :fetcher github
                      :repo "jwiegley/alert"))
    org
    ))


(defun my-windows/init-gntp ()
  "docstring"
;;; Growl notifications for IRC
  (use-package "gntp"
    :init
    (progn
      (setq gntp-icon-path "C:/home/site-lisp/todochiku-icons/"
            gntp-application-icon (concat gntp-icon-path "emacs_32.png")
            gntp-server "localhost"))
    :config
    (progn
      (let ((notifications
             `((alert
                :display "Emacs alert"
                :enabled t
                )
               )))
        (gntp-register notifications gntp-server))))
  )

(defun my-windows/init-alert ()
  (use-package "alert"
    :config
    (progn
      (if (require 'some-library nil 'noerror)
          (setq alert-default-style 'gntp))))
  )

(defun my-windows/post-init-org ()
  (setq org-show-notification-handler (lambda (msg) (alert msg) ))
  )
;;; packages.el ends here
