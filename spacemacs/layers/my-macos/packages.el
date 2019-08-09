;;; packages.el --- my-macos layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Igor Avdeev <ele@MacBook-Pro-Igor.local>
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
;; added to `my-macos-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-macos/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-macos/pre-init-PACKAGE' and/or
;;   `my-macos/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-macos-packages
  '(
    ;; org
    flymake-json
    )
  "The list of Lisp packages required by the my-macos layer.
Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
k    any number of keyword-value-pairs.

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
        recipe.  See: https://gitHub.com/Milkypostman/melpa#recipe-format")


;;; packages.el ends here


 (defun my-macos/init-flymake-json ()
   (use-package flymake-json
     :init
     ;; (spacemacs/set-leader-keys-for-major-mode 'asm-mode "h" 'x86-lookup)
     :config
     (progn
       ;; We need to insert a non-indented line, otherwise it's annoying
       ;; everytime we insert a comment for a routine
       ))
   ;; (global-set-key (kbd "C-c j v") 'flymake-json-load)
  )
