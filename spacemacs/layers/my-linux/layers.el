(mapc 'configuration-layer/declare-layer '(
                                           spell-checking
                                           fasd
                                           lua
                                           (shell :variables
                                                  shell-default-height 30
                                                  shell-default-position 'bottom)
                                           (java :packages (not eclim))
                                           (ranger :variables ranger-override-dired t)
                                           ))
