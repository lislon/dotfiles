;;; packages.el --- my-unused layer packages file for Spacemacs.
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
;; added to `my-unused-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-unused/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-unused/pre-init-PACKAGE' and/or
;;   `my-unused/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-unused-packages
  '(engine-mode)
  "The list of Lisp packages required by the my-unused layer.

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


(defun my-unsed/init-engine-mode ()
  "C-x / g - search-google"
  ;; C-x / g - search google
  (use-package engine-mode
    :config
    (defengine google
      "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
      :keybinding "g")

    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      :keybinding "h")

    (defengine youtube
      "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
      :keybinding "y")

    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      :keybinding "w")
    ))

;;; packages.el ends here
