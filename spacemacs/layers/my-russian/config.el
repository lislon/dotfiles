(defun my/russian-init ()
  (interactive)

  (quail-define-package
   "cyrillic-jcuken" "Cyrillic" "RU" nil
   "ЙЦУКЕH keyboard layout widely used in Russia (ISO 8859-5 encoding)"
   nil t t t t nil nil nil nil nil t)

  (quail-define-rules
   ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8)
   ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=) ("`" ?ё) ("q" ?й) ("w" ?ц) ("e" ?у)
   ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?щ) ("p" ?з) ("[" ?х)
   ("]" ?ъ) ("a" ?ф) ("s" ?ы) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о)
   ("k" ?л) ("l" ?д) (";" ?ж) ("'" ?э) ("\\" ?\\) ("z" ?я) ("x" ?ч) ("c" ?с)
   ("v" ?м) ("b" ?и) ("n" ?т) ("m" ?ь) ("," ?б) ("." ?ю) ("/" ?.) ("!" ?!)
   ("@" ?\") ("#" ?#) ("$" ?\;) ("%" ?%) ("^" ?:) ("&" ??) ("*" ?*) ("(" ?()
                                                                     (")" ?)) ("_" ?_) ("+" ?+) ("~" ?Ё) ("?" ?,)
                                                                     ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш)
                                                                     ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ъ) ("A" ?Ф) ("S" ?Ы) ("D" ?В) ("F" ?А)
                                                                     ("G" ?П) ("H" ?Р) ("J" ?О) ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|" ?/)
                                                                     ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?И) ("N" ?Т) ("M" ?Ь) ("<" ?Б)
                                                                     (">" ?Ю))

  (setq default-input-method "cyrillic-jcuken")
  )

(my/russian-init)

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (local-set-key (kbd "<f13>") 'toggle-input-method)))

;; Fix search with russian language
;;(evil-select-search-module 'evil-search-module 'evil-search)

;; language switching
(global-set-key (kbd "<f13>") 'toggle-input-method)
;; at the begining of working i see message in log:
;; <C-f13> is undefined
(global-set-key (kbd "C-<f13>") 'toggle-input-method)
(define-key special-event-map [sigusr1] 'toggle-input-method)
(with-eval-after-load "auto-dictionary"
    (setq adict-dictionary-list '(("en" "english"
                                   "ru" "russian")))
    (setq adict-hash (let* ((hash (make-hash-table :test 'equal)))
                            (adict-add-word hash 1 "and" "are" "at" "been" "but" "dear" "get" "have"
                                            "he" "hello" "it" "me" "my" "not" "on" "of" "off" "put"
                                            "regarding" "set" "she" "some" "that" "than" "the" "there"
                                            "us" "was" "we" "while" "with" "yes" "you" "your" "yours")

                            (adict-add-word hash 2 "и" "в" "не" "он" "на" "я" "что" "тот" "быть" "с" "а"
                                            "весь" "это" "как" "она" "по" "но" "они" "к" "у" "ты" "из"
                                            "мы" "за" "вы" "так" "же" "от" "сказать" "этот" "который"
                                            "мочь" "человек" "о" "один" "еще" "бы" "такой" "только"
                                            "себя" "свой" "какой" "когда" "уже" "для" "вот" "кто"
                                            "да" "говорить" "год")
                            hash
                       )))
