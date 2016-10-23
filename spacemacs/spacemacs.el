;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; (default nil)
   dotspacemacs-enable-lazy-installation nil
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/dotfiles/spacemacs/private")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; (if (s-starts-with-p "minijack" system-name)
     ;;     'spacemacs-ivy
     ;;   'spacemacs-helm)
     helm
     ;; ivy
     (auto-completion :disabled-for org markdown)
     better-defaults
     emacs-lisp
     common-lisp
     yaml
     search-engine
     ;; smex
     ;; browser-edit
     ;; php
     html
     lua
     c-c++
     shell-scripts
     git
     github
     playground
     colors
     java
     javascript
     python
     gnus
     sql
     php
     docker
     ;; dash ;; zeal help
     ;; commented out because SPC f e R is not work
     ;; ,@(unless (eq system-type 'windows-nt)
     ;;     '(fasd
     ;;       spell-checking))
     fasd
     spell-checking
     ;; mu4e
     ;; eyebrowse
     (semantic :disabled-for emacs-lisp) ;; emacs 25 hang bug
     erc
     emoji
     (ranger :variables ranger-override-dired t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     org
     my-org
     my-keys
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     version-control
     ;; csharp

     ;; gtags
     latex
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(
                                      ;; plantuml
                                      refine
                                      openwith
                                      quickrun
                                      palette
                                      secreteria
                                      ;; todochiku
                                      systemd
                                      tea-time
                                      web-beautify
                                      xelb
                                      undohist
                                      swiper
                                      helm-systemd
                                      ahk-mode
                                      vimrc-mode
                                      bbdb
                                      wttrin
                                      monokai-theme
                                      f3 ;; find wrapper
                                      sos
                                      nyan-mode
                                      restclient
                                      vimgolf
                                      )

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil))


(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported

  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; If non nil activate the TLS certificates verification.
   dotspacemacs-elpa-tls-verification t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(projects agenda)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes (my/select-first-if-day '(leuven monokai spacemacs-dark))
   dotspacemacs-themes '(monokai leuven spacemacs-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; Liberation Mono
   ;; dotspacemacs-default-font '("Terminus"
                               ;; :size 15
                               ;; :weight normal
                               ;; :width normal
                               ;; :powerline-scale 1.5)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
   ;; dotspacemacs-default-font '("Liberation Mono"
   ;;                             :size 15
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-transient-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil' or `any`''. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   )
  ;; User initialization goes here
  (add-to-load-path "~/dotfiles/spacemacs/thirdparty")
  )


(defun dotspacemacs/user-init ()
  ;; Shared undo of org files between computers

  (when (eq system-type 'gnu/linux)
    (setq shared-dir (expand-file-name "~/Dropbox/emacs-bookmarks/cache/undo"))
    (setq-default
     undo-tree-history-directory-alist `(
                                         (,(expand-file-name "~/org") . ,shared-dir)
                                         (,(expand-file-name "~/dotfiles") . ,shared-dir)
                                         (,(expand-file-name "~/.spacemacs") . ,shared-dir)
                                         ("." . ,(expand-file-name "~/.emacs.d/.cache/undo"))))
    )
  (when (eq system-type 'windows-nt)
    ;; repalace process-coding system-alist with from undecided-dos to windows-1251
    ;; this will allow cyrillic characters on windows machines
    (mapc (lambda (val) (setcdr val (cons 'windows-1251 'windows-1251))) process-coding-system-alist)
  ;;   ;; (setq default-process-coding-system '(windows-1251 . windows-1251))
      )
  (setq custom-theme-directory "~/dotfiles/spacemacs/custom-themes")
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; (setq initial-buffer-choice '(lambda () (get-buffer org-agenda-buffer-name)))
  (setq

   ;; basic
   vc-follow-symlinks t
   default-direcotry "~"
   evil-move-beyond-eol nil
   evil-escape-unordered-key-sequence t
   spaceline-org-clock-p t
   x-select-enable-primary t            ; copy/paste from emacs to urxvt
   message-kill-buffer-query nil        ; do not ask confirmation of killing buffer
   ffap-newfile-prompt t                ; gf can create new files
   source-directory "/home/ele/src-dormant/emacs/src"
   avy-case-fold-search nil             ; avy jump respect case
   spacemacs-paste-transient-state-add-bindings '(("p" evil-paste-pop "paste next")
                                                  ("P" evil-paste-pop-next "paste previous"))
   timer-max-repeats 0                  ; Do not repeat timer on suspen
   delete-by-moving-to-trash nil        ; Do not use trash when deleting files
   dired-listing-switches "-alk"
   create-lockfiles nil                 ; get rid of .#filename.org (intented to prevent multiuser editing of file, but I always save files)
   evil-shift-round nil                 ; explanation https://youtu.be/HKF41ivkBb0?t=13m35s


   ;; Dired - directories first
   dired-listing-switches "-alkh  --group-directories-first"
   ;; dired-listing-switches "-aBhl  --group-directories-first"

   ;; Man in same window
   Man-notify-method 'pushy

   ;; java
   eclim-eclipse-dirs "/usr/lib/eclipse"
   eclim-executable "/usr/lib/eclipse/eclim"

   ;; ftp
   ange-ftp-try-passive-mode t

   ;; backup
   make-backup-files t
   keep-new-versions 10
   keep-old-versions 2
   backup-by-copying t
   delete-old-versions t
   version-control t       ;; Always use version numbers in filenames
   backup-directory-alist '(("" . "~/.emacs.d/.cache/backup-per-save"))

   ;; workaround: neotree is painfully slow
   neo-vc-integration nil

   ;; Autocompletion
   auto-completion-complete-with-key-sequence "jk"
   auto-completion-enable-sort-by-usage t
   auto-completion-enable-help-tooltip t

   ;; zsh shell
   comint-input-ring-size 100000
   comint-input-ring-file-name "~/.zsh_history"
                                        ; Ignore timestamps in history file.  Assumes that zsh
                                        ; EXTENDED_HISTORY option is in use.
   comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);"

   ;; IRC
   erc-autojoin-channels-alist
   '(("1\\.0\\.0" "#syl20bnr/spacemacs") ; Gitter
     ("irc.gitter.im" "#syl20bnr/spacemacs" "#syl20bnr/spacemacs-devel")
     ("freenode\\.net" "#emacs"))
   erc-timestamp-format-left "\n%A %B %e, %Y\n\n"
   erc-timestamp-format-right "%H:%M"
   erc-timestamp-right-column 80
   erc-prompt-for-nickserv-password nil
   erc-image-inline-rescale 300
   erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
   erc-foolish-content
   '("\\[Github\\].* starred"
     "\\[Github\\].* forked"
     "\\[Github\\].* synchronize a Pull Request"
     "\\[Github\\].* labeled an issue in"
     "\\[Github\\].* unlabeled an issue in")

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
   my/english-dictionary-file "~/Dropbox/org/static/dictionary.txt"
   ispell-personal-dictionary "~/Dropbox/emacs-resources/ispell-dictionary"
   abbrev-file-name "~/Dropbox/emacs-resources/abbrev_defs.el"

   ;; big undo
   undo-limit (* 10 1000 1000)                    ; 10MB per file
   undo-strong-limit (* 100 1000 1000)            ; 100MB max

   ;; gnus
   gnus-save-killed-list nil
   gnus-always-read-dribble-file t
   gnus-init-file "~/Dropbox/confiles/common/emacs/gnus.el"
   gnus-dribble-directory "~/Dropbox/emacs-resources/gnus/"
   gnus-home-directory "~/Dropbox/emacs-resources/gnus/"

   ;; org-mode timestamps in english
   system-time-locale "C"

   ;; Git fullscreen
   git-magit-status-fullscreen t

   ;; Tea time sound
   tea-time-sound "~/Dropbox/confiles/linux/sounds/131348__kaonaya__bell-at-daitokuji-temple-kyoto.wav"
   ;; Persistent undo
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist
   `(("." . ,(concat spacemacs-cache-directory "undo")))

   ;; scroll margin 3 lines without jumping
   scroll-margin 3
   scroll-conservatively 10000
   scroll-step 1

   ;; Latex
   TeX-engine 'luatex

   ;; quickrun
   quickrun-focus-p nil

   eww-search-prefix "https://www.google.ru/search?q="


   )


  (setq openwith-associations '(("\\.mp4\\'" "mplayer" (file))))
  (openwith-mode t)

  (when (configuration-layer/package-usedp 'ivy)
    ;; (add-to-list ivy-initial-inputs-alist '(counsel-M-x  "^"))
    )
  (when (configuration-layer/package-usedp 'helm)
    ;; (add-to-list ivy-initial-inputs-alist '(counsel-M-x  "^"))
    (setq
     helm-locate-command "locate %s -e %s"
     ;; helm-ff-auto-update-initial-value t
     )
    (global-set-key (kbd "M-a") 'helm-mini)
    ;; recent-f sort minor mode
    (helm-adaptive-mode)
    (evil-leader/set-key "hl" 'helm-locate-library)
    )

  (if (eq system-type 'gnu/linux)
      (setq  tea-time-sound-command "aplay %s"))

  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; q to exit java help
  (with-eval-after-load "eclim-java"
    (loop for map in `(,eclim-java-show-documentation-map)
          do
          (define-key map (kbd "q") 'delete-window)
          (evil-define-key 'normal map (kbd "q") 'delete-window)))


  (with-eval-after-load "quickrun"
    (advice-add 'quickrun :before (lambda (&rest PLIST) (save-buffer))))



  ;; ------------------------------------------------------------------------------
  ;; DEBUG recentf
  ;; ------------------------------------------------------------------------------
  ;; (setq debug-on-quit t)

  (with-eval-after-load "recent"
    (defun recentf-track-opened-file ()
      "Insert the name of the file just opened or written into the recent list."

      (and buffer-file-name (message "recentf track file: %s" buffer-file-name)
           (recentf-add-file buffer-file-name))
      ;; Must return nil because it is run from `write-file-functions'.
      nil))

  ;; (add-hook 'find-file-hook (lambda () (message "find file hook called %s" buffer-file-name)))

  ;; Why do we need this shit?
  ;; (with-eval-after-load "org"
  ;;   (mapcar 'funcall org-store-link-functions))

  ;;------------------------------------------------------------------------------
  ;; Google translate
  ;; ------------------------------------------------------------------------------

  ;; (defun my/google-translate-smooth-translate-reverse()
  ;;   "Mimics google-translate-query-translate with auto-toggling feature"
  ;;   (interactive)
  ;;   (let ((google-translate-translation-directions-alist
  ;;          (append
  ;;           (cdr google-translate-translation-directions-alist)
  ;;           (last google-translate-translation-directions-alist))))
  ;;     (with-temp-buffer
  ;;       (google-translate-smooth-translate))))

  ;; (defun my/google-translate-smooth-translate ()
  ;;   "Mimics google-translate-query-translate-reverse with auto-toggling feature"
  ;;   (interactive)
  ;;   (with-temp-buffer
  ;;     (google-translate-smooth-translate)))
  ;; (spacemacs/set-leader-keys "xgq" 'my/google-translate-smooth-translate)
  ;; (spacemacs/set-leader-keys "xgQ" 'my/google-translate-smooth-translate-reverse)

  ;; Emacs yank C-y in insert mode

  ;; (define-key evil-hybrid-state-map "\C-y" 'yank)

  ;; ;; Use tab in help mode for next button
  ;; (define-key help-mode-map (kbd "<Tab>") 'forward-button)
  (with-eval-after-load 'helm-mode
    (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level))


  ;; (with-eval-after-load 'eww-mode
  ;;   (define-key eww-mode-map (kbd "<Tab>") 'forward-button))
  (add-hook 'eww-mode 'evil-insert-state)
  (add-hook 'yaml-mode 'indent-guide-mode)

  ;; locate shows bad results on archlinux
  (add-hook 'helm-after-initialize-hook (lambda () (setq helm-locate-fuzzy-match nil)))
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'after-save-hook (lambda ()
                               (when (and
                                      (string-prefix-p (concat user-home-directory "bin") buffer-file-name)
                                      (eq major-mode 'sh-mode)
                                      (not (file-executable-p buffer-file-name)))
                                 (message "chmod 755 %s" buffer-file-name)
                                 (set-file-modes buffer-file-name #o755)
                                 )))

  ;; Fixed russian keyboard with proper numbers keys
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

  ;; startundo
  ;; Custom bookmakrs path
  ;; in emacs 24 system-name is FQDN and in emacs 25 it is a hostname
  (setq bookmark-default-file (concat "~/Dropbox/emacs-bookmarks/" (replace-regexp-in-string "\.[^\.]+$" "" (system-name)) "-bookmarks.el"))



  (global-unset-key (kbd "C-h h")) ;; Disable hello file, because it hangs
  (put 'set-goal-column 'disabled nil)  ;; Allow C-x C-n goal column for csv


;;   ;; C-h - updirectory in dired
;;   (evil-define-key 'normal dired-mode-map (kbd "C-S-h") 'dired-up-directory)

;;   ;; Fix RET key in profiler (not works)
;;   ;; (evil-define-key 'normal profiler-report-mode-map (kbd "j") 'profiler-report-toggle-entry)


  (with-eval-after-load 'erc
    ;; IRC
    (add-hook 'erc-insert-pre-hook
              (defun bb/erc-foolish-filter (msg)
                "Ignores messages matching `erc-foolish-content'."
                (when (erc-list-match erc-foolish-content msg)
                  (setq erc-insert-this nil))))
    (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    (erc-track-mode -1)
    (setq erc-insert-modify-hook
          '(erc-controls-highlight
            erc-button-add-buttons
            bb/erc-github-filter
            erc-fill
            erc-match-message
            erc-add-timestamp
            erc-hl-nicks)))

  ;; auto-save all files on lost focus
  (add-hook 'focus-out-hook (lambda () (interactive) (save-some-buffers t)))


  (defvar lsn/gitter-pwd "" "Password in local.el")



  ;; Do I use next keys?
;;   (global-set-key (kbd "C-M-S-x") 'eval-parent-sexp)
;;   ;; (global-set-key (kbd "M-k") 'kill-buffer)
;;   (global-set-key (kbd "C-t") 'yas-insert-snippet)
;;   (define-key yas-minor-mode-map (kbd "C-t") 'yas-insert-snippet)
;;   (global-set-key (kbd "C-h E") 'elisp-index-search)
;;   ;; I tried global-set-key, but it breaks occur in ctrl-s
;;   ;; (global-set-key (kbd "M-s") 'save-buffer)
;;   (define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
;;   (define-key evil-insert-state-map (kbd "M-s") 'save-buffer)

;;   (global-set-key (kbd "C-0") 'delete-window)
;;   (global-set-key (kbd "C-1") 'delete-other-windows)
;;   (global-set-key (kbd "C-2") 'split-window-vertically)
;;   (global-set-key (kbd "C-3") 'split-window-horizontally)
;;   (global-set-key (kbd "C-5") 'make-frame-command)


  ;; I used to C-v for scroll down, not sure about visual block

  ;; at the begining of working i see message in log:
  ;; <C-f13> is undefined
  (global-set-key (kbd "C-<f13>") 'toggle-input-method)

  (use-package tea-time
    :bind (("C-c t" . my/tea-timer)
           ("C-c T" . tea-show-remaining-time))
    :config
    (defun my/tea-timer()
      (interactive)
      (if (file-exists-p tea-time-sound)
          (call-interactively 'tea-time)
        (user-error "Sound not exists at '%s'!" tea-time-sound)))
    )

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (local-set-key (kbd "<f13>") 'toggle-input-method))
              )

  ;; autoscroll messages
  (defadvice message (after message-tail activate)
    "goto point max after a message"
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (walk-windows (lambda (window)
                      (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
                          (set-window-point window (point-max))))
                    nil
                    t)))



  ; Auto set lisp-interaction-mode in *scratch*
  ;; @deprec Now it is dotspacemacs-scratch-mode 'lisp-interaction-mode
  ;; (defun my/scratch-interactive-mode ()
  ;;   (when (equal "*scratch*" (buffer-name))
  ;;     (lisp-interaction-mode)
  ;;     (remove-hook 'window-configuration-change-hook 'my/scratch-interactive-mode))
  ;;   )
  ;; (add-hook 'window-configuration-change-hook 'my/scratch-interactive-mode)

  ;; (add-hook 'calc-mode-hook 'calc-algebraic-entry)

  ;; Using chrome as default browser
  (if (eq system-type 'gnu/linux)
      ;; On minijack opera is default browser
      (with-eval-after-load 'browse-url
        (message "%s default browser changed to chrome. again" (current-time-string))
        (defun browse-url-can-use-xdg-open () (and (getenv "DISPLAY") (executable-find "xdg-open") t))) ;
      ;; (setq browse-url-browser-function 'browse-url-generic
      ;;       browse-url-generic-program "chromium")
      )

  ;; Auto github favore mode when editing markdown
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

  ;; Automode for awesomewm files like rc.lua.blackburg, rc.lua_test
  (add-to-list 'auto-mode-alist '("\\.lua.+" . lua-mode))
  (add-to-list 'auto-mode-alist '("vimrc\\'" . vimrc-mode))

  ;; Shell mode for ~/bin
  (add-to-list 'auto-mode-alist '("rc\\'" . conf-mode))
  (add-to-list 'auto-mode-alist `(,(concat user-home-directory "bin/.+") . shell-script-mode))
  (add-to-list 'auto-mode-alist '("dictionary\\.txt\\'" . google-translate-interactive-mode))


  ;; Add ~/dotfiles/spacemacs/private/snippets directory
  (eval-after-load "yasnippet"
    '(setq yas-snippet-dirs
           (cons
            (expand-file-name (concat
                               (car dotspacemacs-configuration-layer-path)
                               "/snippets"))
            (cdr yas-snippet-dirs))))

  ;; commented because i tired of missing buffer in recentf list after I close it with SPC b d
;;   (with-eval-after-load "recentf"
;;     ;; I want to have  ~/.emacs.d/melpa files in recentf
;;     (delete (expand-file-name package-user-dir) recentf-exclude)

;;     (defsubst file-was-visible-p (file)
;;       "Return non-nil if FILE's buffer exists and has been displayed."
;;       (message "file-was-visible-p %s?" file)
;;       (let ((buf (find-buffer-visiting file)))
;;         (if buf
;;             (let ((display-count (buffer-local-value 'buffer-display-count buf)))
;;               (if (> display-count 0) display-count nil)))))

;;     (let ((r-list recentf-list))
;;       (defun keep-default-old-and-visible-recentf-p (file)
;;         "Decide whether to keep file in recentf-list.
;; Return non-nil if recentf would, by default, keep FILE, and
;; either FILE name was loaded from recentf file on disk or FILE
;; has been displayed in this session."
;;         (if (recentf-keep-default-predicate file)
;;             (if (or (member file r-list)
;;                     (file-was-visible-p file))
;;                 (progn
;;                   (message "File good: %s" file)
;;                   t)
;;               (message "Recentf delete: %s" file)
;;               nil))))

;;     ;; And, of course, you now need:

;;     (setf recentf-keep '(keep-default-old-and-visible-recentf-p)))

  (with-eval-after-load "helm-locate"
    (setq helm-locate-command "locate %s -e %s")
    )

  (with-eval-after-load "systemd"

    (spacemacs/set-leader-keys-for-major-mode 'systemd-mode
      "e" 'my/systemd-enable-unit
      "d" 'my/systemd-disable-unit
      "a" 'my/systemd-start-unit
      "o" 'my/systemd-switch-timer-service
      )

    ;; (add-hook systemd-mode-map)
    )


  (global-set-key (kbd "C-M-S") 'my/isearch-other-window)

  ;; Load local
  (when (file-exists-p "~/local.el")
    (load "~/local.el"))

  (spacemacs/set-leader-keys "cC" 'my/compile)


  ;; Preload org to resume clock in time
  ;; speed up
  (require 'org)

  ;; (with-eval-after-load "Man"
  ;;   (add-hook 'Man-mode-hook (lambda () (pop-to-buffer (current-buffer))))
  ;;   ;; (add-hook 'woman-post-format-hook (lambda () (message "Buffer %s" (buffer-name)))))

  ;; Autoinsert variables Not working + on update on save
  ;; (setq-default auto-insert-directory "~/dotfiles/spacemacs/private/skeletons")
  ;; (auto-insert-mode)
  ;; (define-auto-insert "/\\.emacs.d/.+?\\.el\\'" "spacemacs-skel.el")

  ;; Do not overwrite history on buffer close
  (use-package undohist
    :config (undohist-initialize))

  ;; Extract variable to the line above
  ;; (evil-define-operator my/sh-extract-variable-operator (beg end)
  ;;   :keep-visual t
  ;;   :move-point nil
  ;;   (interactive "<r>")
  ;;   (progn
  ;;     (let ((region (buffer-substring-no-properties beg end))
  ;;           (varname (read-from-minibuffer "Extract variable name: "))
  ;;           )
  ;;       (kill-region beg end)
  ;;       (insert "$" varname)
  ;;       (forward-line -1)
  ;;       (insert varname "=" region)
  ;;       (end-of-line)
  ;;       )
  ;;     ))


  ;; fringes (right tiny column with current cursor) for emacs debug
  (setq-default fringe-indicator-alist
                '((truncation left-arrow right-arrow)
                  (continuation left-curly-arrow right-curly-arrow)
                  (overlay-arrow . right-triangle)
                  (up . up-arrow)
                  (down . down-arrow)
                  (top top-left-angle top-right-angle)
                  (bottom bottom-left-angle
                          bottom-right-angle
                          top-right-angle
                          top-left-angle)
                  (top-bottom left-bracket
                              right-bracket
                              top-right-angle
                              top-left-angle)
                  (empty-line . empty-line)
                  (unknown . question-mark)))

  ;; Fix search with russian language
  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-define-motion evil-search-forward ()
    (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
            (if (and (fboundp 'isearch-forward)
                     (documentation 'isearch-forward))
                (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                        (documentation 'isearch-forward)) ""))
    :jump t
    :type exclusive
    :repeat evil-repeat-search
    (progn                 ;MADE CHANGES HERE
      (evil-insert-state)
      (evil-rch-incrementally t evil-regexp-search)
      (evil-normal-state)
      ))

  (evil-define-motion evil-search-backward ()
    (format "Search forward for user-entered text.
Searches for regular expression if `evil-regexp-search' is t.%s"
            (if (and (fboundp 'isearch-forward)
                     (documentation 'isearch-forward))
                (format "\n\nBelow is the documentation string \
for `isearch-forward',\nwhich lists available keys:\n\n%s"
                        (documentation 'isearch-forward)) ""))
    :jump t
    :type exclusive
    :repeat evil-repeat-search
    (progn                 ;MADE CHANGES HERE
      (evil-insert-state)
      (evil-search-incrementally nil evil-regexp-search)
      (evil-normal-state)
      ))
  (use-package wttrin
    :ensure t
    :commands (wttrin)
    :init
    (evil-leader/set-key "aw" (lambda ()
                                (interactive)
                                (wttrin-query "St.Petersburg")))
    :config
    )
  ;; language switching
  (global-set-key (kbd "<f13>") 'toggle-input-method)
  (define-key special-event-map [sigusr1] 'toggle-input-method)

  ;; make this buffers available for switching via SPC TAB

   (setq spacemacs-useful-buffers-regexp '("\\*SQL*"
                                           "\\*Man*"
                                           "\\*unset mail*"
                                           "\\*Article*"
                                           "\\*Dir\\*"
                                           "\\*info\\*"
                                           "\\*sos\\*"
                                           "\\*new snippet\\*"
                                           "\\*Help\\*"))
  (dotspacemacs/custom-keys)
  (dotspacemacs/hooks)

  ;; I don't know if this works
  (use-package secretaria
    :config
    ;; use this for getting a reminder every 30 minutes of those tasks scheduled
    ;; for today and which have no time of day defined.
    (add-hook 'after-init-hook #'secretaria-today-unknown-time-appt-always-remind-me))

  ;; Fix for mariadb
  ;; TODO: eval-after-load "???" or (Spacemacs) Error in dotspacemacs/user-config: Symbol's function definition is void: sql-set-product-feature
  (with-eval-after-load "sql"
    (setq
     sql-connection-alist `((logiweb
                             (sql-product 'mysql)
                             (sql-user "root")
                             (sql-password ,lsn/mysql-pwd)
                             (sql-server "localhost")
                             (sql-database "logiweb"))))
     (sql-set-product-feature 'mysql :prompt-regexp "^\a?MariaDB[^>]+> "))

  (defun sqli-add-hooks ()
    "My hooks for sql-interactive-mode"
    (spacemacs/toggle-truncate-lines-on)
    )
  (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
    ;; (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")

;;   ;; End of private config
;;   (my/init-swiper)
  (use-package nyan-mode
    :config (nyan-mode))
  )

(defun dotspacemacs/custom-keys ()
  "Here goes my keys customization"

  ;; Vim move right or left
  (my/define-key evil-normal-state-map
    "L" 'evil-end-of-line
    "H" 'evil-first-non-blank
    "C-v" 'evil-scroll-down
    "C-S-v" 'evil-visual-block
    "C-v" 'evil-scroll-down
    "gc" 'org-capture
    "ga" 'org-agenda-list

    ;; paste above/below
    "gp" 'lsn/insert-line-below-and-paste
    "gP" 'lsn/insert-line-above-and-paste
    ;; vim C-a/C-x increase/decrease number
    "+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
    "-" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt
    )

  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "<f8>") 'quickrun)

  ;; Make C-j work with evil in *scratch*
  (evil-define-key 'normal lisp-interaction-mode-map (kbd "C-j") 'my/eval-print-sexp-line)

  (use-package dired
    :bind (:map dired-mode-map
           ("e" . wdired-change-to-wdired-mode))
    )
  (evil-leader/set-key
    "aiq" 'erc-quit-server
    "aid" (defun bb/gitter-debug ()
            (interactive)
            (erc :server "localhost"
                 :port 6667
                 :nick "lislon"
                 :password lsn/gitter-pwd
                 :full-name "lislon"))
    "aig" (defun bb/gitter ()
            (interactive)
            (erc-tls :server "irc.gitter.im"
                     :port 6667
                     :nick "lislon"
                     :password lsn/gitter-pwd
                     :full-name "lislon"))
    "aif" (defun bb/freenode ()
            (interactive)
            (erc :server "irc.freenode.net"
                 :port "6667"
                 :nick "lislon"
                 :full-name "lislon"))
    "am" 'my/sql-connect-preset          ; connect to mysql
    "fd" 'my/ediff-buffer-with-file
    "oJ" 'my/java-text
    "xgi" 'my/google-translate-repl
    "os" 'yas-visit-snippet-file
    "oi" (lambda () (interactive) (error "Use SPC i s to insert yasnippet"))
    ;; "oS" 'yas-new-snippet
    "oS" (lambda () (interactive) (error "Use SPC i S c to create yasnippet"))
    "az" 'shell
    "hs" 'sos                           ; Stackoverflow help
    "os" 'just-one-space
    )

  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
    "rv" 'my/sh-extract-variable)

  ;; documentation for snippet mode
  (spacemacs/set-leader-keys-for-major-mode
    'snippet-mode "h" (lambda () (interactive)
                        (browse-url "http://joaotavora.github.io/yasnippet/snippet-development.html")))

  ;; connect to mysql via SPC m c
  ;; (spacemacs/set-leader-keys-for-major-mode 'sql-mode "c" 'my/sql-connect-preset)
)

(defun dotspacemacs/hooks ()
  "Here goes my keys customization"
  (add-hook 'palette-mode-hook (lambda ()
                                 (highlight-)))
  ;; java multiline comments
  (add-hook 'java-hook (lambda () (local-set-key "\r" 'my-javadoc-return)))
  ;; Beyond eol makes ~SPC m e e~ usable
  (add-hook 'lisp-mode-hook ((lambda () (setq-local evil-move-beyond-eol t))))
  (add-hook 'auto-save-hook 'auto-save-all-files)

  (add-hook 'systemd-mode-hook (lambda () (setq-local comment-start-skip "#")))
  (add-hook 'shell-mode-hook 'my-shell-mode-hook)
  (add-hook 'nxml-mode-hook 'turn-on-evil-matchit-mode)
  (add-hook 'ranger-mode-hook (lambda () (face-remap-add-relative 'hl-line '((:background "DimGray")))))
  )


;; TODO implement this
 ; swap 1 and 2
(setq custom-file (expand-file-name "~/Dropbox/dotfiles/spacemacs/emacs-custom.el"))
(load custom-file)
(load "~/Dropbox/dotfiles/spacemacs/funcs.el")
