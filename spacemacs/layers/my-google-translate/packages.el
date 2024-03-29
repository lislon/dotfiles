;;; packages.el --- my-google-translate layer packages file for Spacemacs.
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
;; added to `my-google-translate-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-google-translate/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-google-translate/pre-init-PACKAGE' and/or
;;   `my-google-translate/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-google-translate-packages
  '(google-translate)
  "The list of Lisp packages required by the my-google-translate layer.

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


(defun my-google-translate/post-init-google-translate ()
  (setq
   ;; Google translate
   google-translate-default-source-language "en"
   google-translate-default-target-language "ru"
   google-translate-input-method-auto-toggling t
   google-translate-preferable-input-methods-alist
   '((nil . ("en"))
     ("cyrillic-jcuken" . ("ru")))
   google-translate-translation-directions-alist
   '(("en" . "ru") ("ru" . "en") )
   google-translate-pop-up-buffer-set-focus t
   my/english-dictionary-file "~/OneDrive/org/static/dictionary.txt")
  )
(defvar my/english-dictionary-file nil "Google translate dictionary file")

;;; packages.el ends here
