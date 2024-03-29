;;; packages.el --- my-skillbox layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  <IAvdeev@SPB-IAVDEEV>
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
;; added to `my-skillbox-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-skillbox/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-skillbox/pre-init-PACKAGE' and/or
;;   `my-skillbox/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-skillbox-packages
  '(helm
    request
    )
  "The list of Lisp packages required by the my-skillbox layer.

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

(defun my-skillbox/post-init-request ()
  (use-package request
    ;;    :demand t
    :defer t
  ))

(defun my-skillbox/post-init-helm ()
  (message "my-org/post-init-helm")
  (use-package helm
    :defer t
    :config
    (evil-leader/set-key "oi" 'my-skillbox//helm-answer-templates)
    (evil-leader/set-key "ot" 'my-skillbox//opentask)
    (evil-leader/set-key "on" 'my-skillbox/new-check)
    (evil-leader/set-key "og" 'my-skillbox//git-pull)
    (evil-leader/set-key "ir" 'my-skillbox/copy-string-for-report)
    (evil-leader/set-key "oe" 'my-skillbox/extract-file-to-current-module)


    (add-hook 'org-mode-hook 'my-skillbox//org-hook)
    ;; (setq org-export-global-macros '(("OK". "@@html:<font color='green'>Зачет 🎄</font>@@")
                                     ;; ("FAIL". "@@html:<font color='blue'>Доработать ☕</font>@@")))
    ;; (setq org-export-global-macros '(("OK". "@@html:<font color='green'>Зачет 👍</font>@@")
                                     ;; ("FAIL". "@@html:<font color='blue'>Доработать ☕</font>@@")))

    (setq org-export-global-macros '(("OK". "@@html:<span style=\"color: white; padding: 0.1em 0.3em; border-radius: 3px; background-clip: padding-box; font-size: 14.4px; font-family: &quot;Lucida Console&quot;, monospace; line-height: 1; background-color: #23af11;\">Принято</span>@@")
                                     ("FAIL". "@@html:<span style=\"background-color: #3d3bff; color: white; padding: 0.1em 0.3em; border-radius: 3px; background-clip: padding-box; font-size: 14.4px; font-family: &quot;Lucida Console&quot;, monospace; line-height: 1;\">В доработке</span>@@")))

    )
  (with-eval-after-load "keybindings.el"
    (spacemacs/toggle-visual-line-navigation-on))
  )

(define-minor-mode skillbox-mode
  "Skillbox mode"
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Skillbox"
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-q") (lambda () (interactive)
                                          (end-of-line)
                                          (newline)
                                          (insert "#+BEGIN_QUOTE\n\n#+END_QUOTE")
                                          (forward-line -1)
                                          ))
            (define-key map (kbd "C-s") #'my-skillbox//insert-code-block)
            (define-key map (kbd "<f1>") #'my-skillbox//mark-job-success)
            (define-key map (kbd "<f2>") #'my-skillbox//mark-job-fail)
            map)
  :after-hook (progn
                (make-local-variable 'my-skillbox//module)
                (when (buffer-file-name)
                  (setq my-skillbox//module (and (string-match "/\\([0-9]+\\)[./]" (buffer-file-name)) (match-string 1 (buffer-file-name)))))

                (make-local-variable 'org-html-postamble)
                (let ((feedback-url "https://forms.gle/UQsgozushdPS7hms9")
                      (good-feedback-url "https://forms.gle/We9mZSCGiN4AtPQK6"))
                  (setq org-html-postamble
                        (concat
                         "Игорь"
                         )
                        )
                  ;; (setq org-html-postamble
                  ;;       (concat
                  ;;        "С Уважением, Игорь"
                  ;;        "<br ><small><a href=\""
                  ;;        feedback-url
                  ;;        "\">Мой ответ мог быть лучше?</a>"
                  ;;        " / "
                  ;;        "<a href=\""
                  ;;        good-feedback-url
                  ;;        "\">Отзывы</a>"

                  ;;        "</small>"
                  ;;        )
                  ;;       )
                  )

                ;; better code snippet coloring
                (add-hook 'org-export-before-processing-hook 'my-skillbox//org-inline-css-hook)

                (setq org-export-preserve-breaks t)
                (setq org-html-wrap-src-lines t); on windows it will help with linebraks in <pre>
                ;; keymap precedence
                ;; (add-to-list 'emulation-mode-map-alists `((skillbox-mode . ,skillbox-mode-map)))
                )
  :group 'skillbox)

(defun my-skillbox//org-hook ()
  "Set local variable"
  (when (and (buffer-file-name) (string-match-p (regexp-quote "shared/skillbox") (buffer-file-name))))
    (add-hook 'skillbox-mode-hook 'flyspell-mode)
    (skillbox-mode)
    (global-set-key (kbd "<f1>") 'my-skillbox//mark-job-success)
    (global-set-key (kbd "<f2>") 'my-skillbox//mark-job-fail)
  )

(defun my-skillbox//opentask()
  "docstring"
  (interactive)
  ;; (split-window-horizontally)
  (find-file-other-window (concat my-skillbox//base-dir "/all/" my-skillbox//module "/task" my-skillbox//module ".org"));
  )


(defun my-skillbox//mark-job-success ()
  (interactive)
  (save-excursion
    (my-skillbox//replace-title "{{{OK}}}")))

(defun my-skillbox//mark-job-fail ()
  (interactive)
  (save-excursion
    (my-skillbox//replace-title "{{{FAIL}}}")))

(defun my-skillbox//replace-title (text)
(search-backward "**")
  (push-mark)
  (search-backward " -")
  (forward-char 2)
  (delete-region (mark) (point))
  (insert " ")
  (insert text))

;;; packages.el ends here
;; (org-set-generic-type
;;  "really-basic-text"
;;  '(:body-section-header-prefix	("<b>" "<h2>" "<h3>" "<h4>" "<h5>" "<h6>")
;;    :body-section-header-suffix	("</b>\n" "</h2>\n" "</h3>\n" "</h4>\n" "</h5>\n" "</h6>\n")
;;                     ))
