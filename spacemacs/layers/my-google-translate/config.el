
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
  my/english-dictionary-file "~/Dropbox/org/static/dictionary.txt")


(evil-leader/set-key
  "xgi" 'my/google-translate-repl)

