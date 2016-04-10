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
     spacemacs-helm
     ;; spacemacs-ivy
     auto-completion
     better-defaults
     emacs-lisp
     common-lisp
     ;; browser-edit
     chrome
     html
     lua
     ;; c-c++
     shell-scripts
     git
     github
     playground
     colors
     java
     javascript
     python
     gnus
     ;; eyebrowse
     semantic
     erc
     emoji
     xkcd
     ;; markdown
     org
     my-org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     syntax-checking
     version-control
     csharp
     spell-checking
     gtags
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(
                                      gnus-desktop-notify
                                      quickrun
                                      ;; todochiku
                                      systemd
                                      tea-time
                                      web-beautify
                                      xelb
                                      undohist
                                      )

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
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
  ;; spacemacs settings.
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
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents bookmarks agenda)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         dream
                         wombat
                         solarized-light
                         leuven
                         monokai
                         zenburn)
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
   dotspacemacs-default-font '("Liberation Mono"
                               :size 15
                               :weight normal
                               :width normal
                               :powerline-scale 1.5)
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
   dotspacemacs-auto-resume-layouts t
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
  )

;; ============================================================
(defun abbreviate-file-name (filename)
  "Return a version of FILENAME shortened using `directory-abbrev-alist'.
This also substitutes \"~\" for the user's home directory (unless the
home directory is a root directory) and removes automounter prefixes
\(see the variable `automount-dir-prefix')."
  ;; Get rid of the prefixes added by the automounter.
  (if (not filename)
      (debug)
      )
  (save-match-data
    (if (and automount-dir-prefix
	     (string-match automount-dir-prefix filename)
	     (file-exists-p (file-name-directory
			     (substring filename (1- (match-end 0))))))
	(setq filename (substring filename (1- (match-end 0)))))
    ;; Avoid treating /home/foo as /home/Foo during `~' substitution.
    ;; To fix this right, we need a `file-name-case-sensitive-p'
    ;; function, but we don't have that yet, so just guess.
    (let ((case-fold-search
	   (memq system-type '(ms-dos windows-nt darwin cygwin))))
      ;; If any elt of directory-abbrev-alist matches this name,
      ;; abbreviate accordingly.
      (dolist (dir-abbrev directory-abbrev-alist)
	(if (string-match (car dir-abbrev) filename)
	    (setq filename
		  (concat (cdr dir-abbrev)
			  (substring filename (match-end 0))))))
      ;; Compute and save the abbreviated homedir name.
      ;; We defer computing this until the first time it's needed, to
      ;; give time for directory-abbrev-alist to be set properly.
      ;; We include a slash at the end, to avoid spurious matches
      ;; such as `/usr/foobar' when the home dir is `/usr/foo'.
      (or abbreviated-home-dir
	  (setq abbreviated-home-dir
		(let ((abbreviated-home-dir "$foo"))
		  (concat "\\`" (abbreviate-file-name (expand-file-name "~"))
			  "\\(/\\|\\'\\)"))))

      ;; If FILENAME starts with the abbreviated homedir,
      ;; make it start with `~' instead.
      (if (and (string-match abbreviated-home-dir filename)
	       ;; If the home dir is just /, don't change it.
	       (not (and (= (match-end 0) 1)
			 (= (aref filename 0) ?/)))
	       ;; MS-DOS root directories can come with a drive letter;
	       ;; Novell Netware allows drive letters beyond `Z:'.
	       (not (and (memq system-type '(ms-dos windows-nt cygwin))
			 (save-match-data
			   (string-match "^[a-zA-`]:/$" filename)))))
	  (setq filename
		(concat "~"
			(match-string 1 filename)
			(substring filename (match-end 0)))))
      filename)))

(defun dotspacemacs/user-init ()
  ;; Shared undo of org files between computers
  (when (eq system-type 'gnu/linux)
    (setq shared-dir (expand-file-name "~/Dropbox/emacs-bookmarks/cache/undo"))
    (setq-default
     undo-tree-history-directory-alist `(
                                         (,(expand-file-name "~/org") . ,shared-dir)
                                         (,(expand-file-name "~/dotfiles") . ,shared-dir)
                                         (,(expand-file-name "~/.spacemacs") . ,shared-dir)
                                         ("." . ,(expand-file-name "~/.emacs.d/.cache/undo"))
                                         ))
    )
  (setq custom-theme-directory "~/dotfiles/spacemacs/custom-themes")
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq initial-buffer-choice '(lambda () (get-buffer org-agenda-buffer-name)))
  (setq

   ;; basic
   vc-follow-symlinks t
   default-direcotry "~"
   evil-move-beyond-eol nil
   evil-escape-unordered-key-sequence t
   spaceline-org-clock-p t
   x-select-enable-primary t            ; copy/paste from emacs to urxvt
   helm-locate-command "locate %s -e %s"
   message-kill-buffer-query nil        ; do not ask confirmation of killing buffer
   ffap-newfile-prompt t                ; gf can create new files
   source-directory "/home/ele/src/emacs/src"
   avy-case-fold-search nil             ; avy jump respect case
   spaceline-org-clock-p t
   spacemacs-paste-transient-state-add-bindings '(("p" evil-paste-pop-next "paste next")
                                                  ("P" evil-paste-pop "paste previous"))
   timer-max-repeats 0                  ; Do not repeat timer on suspen


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
   auto-completion-enable-snippets-in-popup t
   auto-completion-enable-help-tooltip t

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
   google-translate-referable-input-methods-alist
   '((nil . ("en"))
     ("cyrillic-jcuken" . ("ru")))
   google-translate-translation-directions-alist
   '(("en" . "ru") ("ru" . "en") )
   google-translate-pop-up-buffer-set-focus t

   ;; big undo
   undo-limit 200000                    ; 200kb per file
   undo-strong-limit 500000             ; 500kb max

   ;; gnus
   gnus-select-method '(nntp "news.gmane.org")
   gnus-save-killed-list nil
   gnus-always-read-dribble-file t

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

   spaceline-org-clock-p t
   )

  (if (eq system-type 'gnu/linux)
      (setq  tea-time-sound-command "aplay %s"))

  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; M-\ to escape from insert mode (from russian language)
  (define-key evil-hybrid-state-map (kbd "M-\\") 'evil-escape)

  ;; Vim move right or left
  (define-key evil-normal-state-map "L" 'evil-end-of-line)
  (define-key evil-normal-state-map "H" 'spacemacs/smart-move-beginning-of-line)

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

  ;; locate shows bad results on archlinux
  (add-hook 'helm-after-initialize-hook (lambda () (setq helm-locate-fuzzy-match nil)))

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
  (setq bookmark-default-file (concat "~/Dropbox/emacs-bookmarks/" system-name "-bookmarks.el"))


  ;; Disable hello file, because it hangs
  (global-unset-key (kbd "C-h h"))
  ;; Allow C-x C-n goal column for csv
  (put 'set-goal-column 'disabled nil)

  ;; Fix tab key
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)

  ;; C-h - updirectory in dired
  (evil-define-key 'normal dired-mode-map (kbd "C-S-h") 'dired-up-directory)

  ;; Fix RET key in profiler (not works)
  ;; (evil-define-key 'normal profiler-report-mode-map (kbd "j") 'profiler-report-toggle-entry)

  ;; IRC
  (add-hook 'erc-insert-pre-hook
            (defun bb/erc-foolish-filter (msg)
              "Ignores messages matching `erc-foolish-content'."
              (when (erc-list-match erc-foolish-content msg)
                (setq erc-insert-this nil))))

  (defun bb/erc-github-filter ()
    "Shortens messages from gitter."
    (interactive)
    (when (and (< 18 (- (point-max) (point-min)))
               (string= (buffer-substring (point-min)
                                          (+ (point-min) 18))
                        "<gitter> [Github] "))
      (dolist (regexp '(" \\[Github\\]"
                        " \\(?:in\\|to\\) [^ /]+/[^ /:]+"))
        (goto-char (point-min))
        (when (re-search-forward regexp (point-max) t)
          (replace-match "")))
      (goto-char (point-min))
      (when (re-search-forward
             "https?://github\\.com/[^/]+/[^/]+/[^/]+/\\([[:digit:]]+\\)\\([^[:space:]]*\\)?"
             (point-max) t)
        (let* ((url (match-string 0))
               (number (match-string 1))
               (start (+ 1 (match-beginning 0)))
               (end (+ 1 (length number) start)))
          (replace-match (format "(#%s)" (match-string 1)))
          (erc-button-add-button start end 'browse-url nil (list url)))
        )))

  (with-eval-after-load 'erc
    (setq erc-insert-modify-hook
          '(erc-controls-highlight
            erc-button-add-buttons
            bb/erc-github-filter
            erc-fill
            erc-match-message
            erc-add-timestamp
            erc-hl-nicks)))

  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)

  (with-eval-after-load 'erc
    (erc-track-mode -1))


  (defvar lsn/gitter-pwd "" "Password in local.el")

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
                 :full-name "lislon")))

  (evil-leader/set-key
    "fd" 'my/ediff-buffer-with-file)

  (defvar my/org-mobile-sync-timer nil)
  (defvar my/org-mobile-sync-secs (* 60 60 24))

  (defun my/org-mobile-sync-pull-and-push ()
    (require 'org)
    (org-mobile-pull)
    (org-mobile-push)
    (when (fboundp 'sauron-add-event)
      (sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))

  (defun my/org-mobile-sync-start ()
    "Start automated `org-mobile-push'"
    (interactive)
    (setq my/org-mobile-sync-timer
          (run-with-timer my/org-mobile-sync-secs my/org-mobile-sync-secs
                          'my/org-mobile-sync-pull-and-push)))

  (defun my/org-mobile-sync-stop ()
    "Stop automated `org-mobile-push'"
    (interactive)
    (cancel-timer my/org-mobile-sync-timer))

  (my/org-mobile-sync-start)

  (defun my/org-mobile-fix-index-bug ()
    "Fixes MobileOrg's index.org after push to workaround bug in Android.
That function deletes \"#+ALLPRIORITIES\" line from index.org file"
    (interactive)
    (let ((file (concat org-mobile-directory "/index.org")))
      (save-excursion
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (when (search-forward "#+ALLPRIORITIES" nil t)
            ;; Avoid polluting kill-ring by not calling (kill-line)
            (let ((beg (progn (forward-line 0)
                              (point))))
              (forward-line 1)
              (delete-region beg (point))))
          (write-region nil nil file)
          )
        )))
  (advice-add 'org-mobile-push :after 'my/org-mobile-fix-index-bug)


  ;; Google translate interactive mode
  (define-derived-mode google-translate-interactive-mode
    text-mode "Google Translate"
    (defun my/next-line-empty-p ()
      "Check if next line empty"
      (save-excursion
        (beginning-of-line 2)
        (save-match-data
          (looking-at "[ \t]*$"))
        ))
    (defun my/translate-word-and-next-line ()
      "Shows translation of current line in help buffer and inserts
          new line after it"
      (interactive)
      (let ((buffer (current-buffer)) )
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (move-end-of-line nil)
        (google-translate-at-point)
        (switch-to-buffer buffer)
        (if (eq (point) (point-max))
            (newline-and-indent)
          (end-of-line 2))
        ))

    (use-local-map (make-sparse-keymap))
    (local-set-key (kbd "RET") 'my/translate-word-and-next-line)
    (evil-local-set-key 'normal (kbd "RET") 'my/translate-word-and-next-line))

  (defun my/google-translate-repl ()
    (interactive)
    (require 'google-translate-default-ui)
    (let ((buffer (get-buffer-create "Google Translate REPL")))
      (switch-to-buffer buffer)
      (google-translate-interactive-mode)
      (evil-insert-state)
      (goto-char (buffer-end 1))
      ))

  (defun lsn-insert-line-and-paste (count)
    "Moves to new line and paste text"
    (interactive "P")
    (move-end-of-line nil)
    (newline)
    (evil-paste-after count))

  (defun my/keys-help-sheet ()
    "Move to keys cheat sheet"
    (interactive)
    (find-file "~/org/keys.org")
    )

  (evil-leader/set-key "x g i" 'my/google-translate-repl)
  ;; SPC o k - Show cheatsheet with hotkeys
  (evil-leader/set-key "ok" 'my/keys-help-sheet)
  (evil-leader/set-key "os" 'yas-visit-snippet-file)
  (evil-leader/set-key "oS" 'yas-new-snippet)
  (evil-leader/set-key "az" 'shell)
  (define-key evil-normal-state-map (kbd "gp") 'lsn-insert-line-and-paste)
  (add-hook 'dired-mode-hook (lambda ()
                               (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

  (defun eval-parent-sexp ()
    "Cause sometimes you just want to eval just the immediate
form. not the top level, but without going to the closing paren
and evaling there."
    (interactive)
    (save-excursion
      ;; get out of string if in it
      (dotimes (c (if (in-string-p) 2 1))
        (up-list+))
      (let ((cmd (key-binding (kbd "C-x C-e"))))
        (if (eq cmd 'slime-eval-last-expression)
            (funcall cmd)
          (funcall cmd '())))))

  (global-set-key (kbd "C-M-S-x") 'eval-parent-sexp)
  (global-set-key (kbd "M-a") 'helm-mini)
  (global-set-key (kbd "M-k") 'kill-buffer)
  (global-set-key (kbd "C-h E") 'elisp-index-search)
  ;; I tried global-set-key, but it breaks occur in ctrl-s
  ;; (global-set-key (kbd "M-s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "M-s") 'save-buffer)

  (global-set-key (kbd "C-0") 'delete-window)
  (global-set-key (kbd "C-1") 'delete-other-windows)
  (global-set-key (kbd "C-2") 'split-window-vertically)
  (global-set-key (kbd "C-3") 'split-window-horizontally)
  (global-set-key (kbd "C-5") 'make-frame-command)


  ;; I used to C-v for scroll down, not sure about visual block
  (define-key evil-normal-state-map (kbd "C-v") 'evil-scroll-down)
  (define-key evil-motion-state-map (kbd "C-v") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-S-v") 'evil-visual-block)

  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

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

  ;; (global-set-key (kbd "C-c t") (defun my/tea-timer()
  ;;                                 (interactive)
  ;;                                 (require 'tea-time)
  ;;                                 (if (file-exists-p tea-time-sound)
  ;;                                     (call-interactively 'tea-time)
  ;;                                   (user-error "Sound not exists at '%s'!" tea-time-sound))))
  ;; (global-set-key (kbd "C-c T") 'tea-show-remaining-time)

  (global-set-key (kbd "<f8>") 'quickrun)
  (message "F13 here")
  (global-set-key (kbd "<f13>") 'toggle-input-method)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (local-set-key (kbd "<f13>") 'toggle-input-method)))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (local-set-key (kbd "<f12>") 'toggle-input-method)))

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

  (setq quickrun-focus-p nil)

  ;; recent-f sort minor mode
  (helm-adaptive-mode)

  ;; (add-hook 'evil-hybrid-state-entry-hook
  ;;           (lambda () (literal-insert-mode 1)))
  ;; (add-hook 'evil-hybrid-state-exit-hook
  ;;           (lambda () (literal-insert-mode -1)))

  ;; keep navigation when russian keyboard is active
  (defun translate-keystrokes-ru-en ()
    "Make emacs output english characters, regardless whether
the OS keyboard is english or russian"
    (flet ((make-key-stroke (prefix char)
                            (eval `(kbd ,(if (and (string-match "^C-" prefix)
                                                  (string-match "[A-Z]" (string char)))
                                             (concat "S-" prefix (string (downcase char)))
                                           (concat prefix (string char)))))))
      (let ((case-fold-search nil)
            (keys-pairs (mapcar* 'cons
                                 "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
                                 "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
            (prefixes '(""    "s-"    "M-"    "M-s-"
                        "C-"  "C-s-"  "C-M-"  "C-M-s-")))
        (mapc (lambda (prefix)
                (mapc (lambda (pair)
                        (define-key key-translation-map
                          (make-key-stroke prefix (car pair))
                          (make-key-stroke prefix (cdr pair))))
                      keys-pairs))
              prefixes))))

  ;; (translate-keystrokes-ru-en)
  (defun literal-insert ()
    (interactive)
    (insert-char last-input-event 1))


  (define-minor-mode literal-insert-mode
    "Make emacs output characters corresponging to the OS keyboard,
 ignoring the key-translation-map"
    :keymap (let ((new-map (make-sparse-keymap))
                  (english-chars "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"))
              (mapc (lambda (char)
                      (define-key new-map (string char)
                        'literal-insert))
                    english-chars)
              new-map))

                                        ; Auto set lisp-interaction-mode in *scratch*
  ;; @deprec Now it is dotspacemacs-scratch-mode 'lisp-interaction-mode
  ;; (defun my/scratch-interactive-mode ()
  ;;   (when (equal "*scratch*" (buffer-name))
  ;;     (lisp-interaction-mode)
  ;;     (remove-hook 'window-configuration-change-hook 'my/scratch-interactive-mode))
  ;;   )
  ;; (add-hook 'window-configuration-change-hook 'my/scratch-interactive-mode)

  ;; Beyond eol makes ~SPC m e e~ usable
  (add-hook 'lisp-mode-hook ((lambda () (setq-local evil-move-beyond-eol t))))
  ;; (add-hook 'calc-mode-hook 'calc-algebraic-entry)

  ;; Using chrome as default browser
  (if (eq system-type 'gnu/linux)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "chromium"))

  ;; Auto github favore mode when editing markdown
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

  ;; Automode for awesomewm files like rc.lua.blackburg, rc.lua_test
  (add-to-list 'auto-mode-alist '("\\.lua.+" . lua-mode))

  ;; Add ~/dotfiles/spacemacs/private/snippets directory
  (eval-after-load "yasnippet"
    '(setq yas-snippet-dirs
           (cons 
            (expand-file-name (concat
                               (car dotspacemacs-configuration-layer-path)
                               "/snippets"))
            (cdr yas-snippet-dirs))))

  (with-eval-after-load "recentf"
    ;; Remove ~/.emacs.d/melpa from ignore list
    (delete (expand-file-name package-user-dir) recentf-exclude)

    (defsubst file-was-visible-p (file)
      "Return non-nil if FILE's buffer exists and has been displayed."
      (let ((buf (find-buffer-visiting file)))
        (if buf
            (let ((display-count (buffer-local-value 'buffer-display-count buf)))
              (if (> display-count 0) display-count nil)))))

    (let ((r-list recentf-list))
      (defun keep-default-old-and-visible-recentf-p (file)
        "Decide whether to keep file in recentf-list.
Return non-nil if recentf would, by default, keep FILE, and
either FILE name was loaded from recentf file on disk or FILE
has been displayed in this session."
        (if (recentf-keep-default-predicate file)
            (if (or (member file r-list)
                    (file-was-visible-p file))
                (progn
                  (message "File good: %s" file)
                  t)
              nil))))

    ;; And, of course, you now need:
    (setf recentf-keep '(keep-default-old-and-visible-recentf-p)))

  (with-eval-after-load "browse-url"
    ;; override `browse-url-can-use-xdg-open'; does not work on my awesomewm setup
    (defun browse-url-can-use-xdg-open ()
      (and (getenv "DISPLAY") (getenv "XDG_SESSION_ID"))))

  (with-eval-after-load "helm-locate"
    (setq helm-locate-command "locate %s -e %s")
    )

  (with-eval-after-load "systemd"
    (defun my/systemd-enable-unit ()
      (interactive)
      (shell-command (format  "systemctl --user enable %s" (buffer-name))))

    (defun my/systemd-disable-unit ()
      (interactive)
      (shell-command (format  "systemctl --user disable %s" (buffer-name))))

    (defun my/systemd-start-unit ()
      (interactive)
      (shell-command (format  "systemctl --user start %s" (buffer-name))))

    (defun my/systemd-switch-timer-service ()
      "Switch between service/timer files"
      (interactive)
      (let* ((base-name (file-name-sans-extension
                         (buffer-file-name)))
             (service-name (concat base-name ".service"))
             (timer-name (concat base-name ".timer")))
        (cond
         ((and (string= (buffer-file-name) service-name)
               (file-exists-p timer-name))
          (find-file timer-name))
         ((and (string= (buffer-file-name) timer-name)
               (file-exists-p service-name))
          (find-file service-name)))))

    (defun my/systemd-move-to-system ()
      "Move systemd files to system"
      (interactive)
      (let* ((base-name (file-name-sans-extension
                         (buffer-file-name)))
             (service-name (concat base-name ".service"))
             (timer-name (concat base-name ".timer")))
        (when (s-starts-with-p "/home")
          ()
          )
        (cond
         ((and (string= (buffer-file-name) service-name)
               (file-exists-p timer-name))
          (find-file timer-name))
         ((and (string= (buffer-file-name) timer-name)
               (file-exists-p service-name))
          (find-file service-name))))
      )

    (spacemacs/set-leader-keys-for-major-mode 'systemd-mode
      "e" 'my/systemd-enable-unit
      "d" 'my/systemd-disable-unit
      "a" 'my/systemd-start-unit
      "o" 'my/systemd-switch-timer-service
      )

    ;; (add-hook systemd-mode-map)
    )

  (defun my/spacemacs-maybe-kill-emacs ()
    "If emacs server is running, kills frame instead of server"
    (interactive)
    (if (server-running-p)
        (spacemacs/frame-killer)
      (spacemacs/kill-emacs)))

  ;; I don't want close emacs daemon by SPC q q
  (evil-leader/set-key "qq" 'my/spacemacs-maybe-kill-emacs)

  (defun my/isearch-other-window ()
    "Search in other window"
    (interactive)
    (save-selected-window
      (other-window 1)
      (isearch-forward)))

  (global-set-key (kbd "C-M-S") 'my/isearch-other-window)

  ;; (defun my/evil-visual-restore (func &rest args)
  ;;   (if (eq last-command evil-paste-after)
  ;;       )
  ;;   )

  ;; (advice-add 'evil-visual-restore :around 'my/org-mobile-fix-index-bug)

  ;; Load local
  (when (file-exists-p "~/local.el")
    (load "~/local.el"))

  (defun my/compile ()
    "Compile program. With prefix arg change compile args"
    (interactive)
    (setq-local compilation-read-command nil)
    (call-interactively 'compile)
    )

  (spacemacs/set-leader-keys "cC" 'my/compile)

  ;; (spacemacs/set-leader-keys "otC" (defun trello-sync-in () (interactive) (org-trello/sync-buffer '(4))))
  ;; (spacemacs/set-leader-keys "otc" (defun trello-sync-out () (interactive) (org-trello/sync-buffer)))

  (evil-leader/set-key "hl" 'helm-locate-library)
  ;; Preload org to resume clock in time
  ;; speed up
  (require 'org)


  ;; (with-eval-after-load "Man"
  ;;   (add-hook 'Man-mode-hook (lambda () (pop-to-buffer (current-buffer))))
  ;;   ;; (add-hook 'woman-post-format-hook (lambda () (message "Buffer %s" (buffer-name)))))
  (define-key evil-normal-state-map  (kbd "g r") 'jump-to-register)
  (define-key evil-normal-state-map  (kbd "g c") 'org-capture)
  (define-key evil-normal-state-map  (kbd "g a") 'org-agenda-list)

  ;; Autoinsert variables Not working + on update on save
  ;; (setq-default auto-insert-directory "~/dotfiles/spacemacs/private/skeletons")
  ;; (auto-insert-mode)
  ;; (define-auto-insert "/\\.emacs.d/.+?\\.el\\'" "spacemacs-skel.el")

  ;; Do not overwrite history on buffer close
  (require 'undohist)
  (undohist-initialize)


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

  (defun my/sh-extract-variable (varname)
    "Extract WORD under cursor to a variable"
    (interactive "sExtract variable name: ")

    (let* ((inner-word (evil-inner-WORD))
           (beg (car inner-word)) ;; inner-word[0]
           (end (cadr inner-word)) ;; inner-word[1]
           (region (buffer-substring-no-properties beg end)))
      (kill-region beg end)
      (insert "$" varname)
      (forward-line -1)
      (newline-and-indent)
      (insert varname "=" region)
      (end-of-line)
      )
    )


  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
    "rv" 'my/sh-extract-variable)
  (add-hook 'systemd-mode-hook (lambda () (setq-local comment-start-skip "#")))

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
      (evil-search-incrementally t evil-regexp-search)
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
      (evil-search-incrementally t evil-regexp-search)
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
  ;; End of private config
  )

(defun my/systemd-user-timer-create (systemd-name)
  (interactive "sUnit name: ")
  (find-file (concat user-home-directory "/.config/systemd/user/" systemd-name ".timer"))
  (yas-minor-mode)
  (yas-insert-snippet "timer")
  ;; (insert "timer")
  ;; (find-file (concat user-home-directory "/.config/systemd/user/" systemd-name ".service")
  ;; (insert "service")
)

(defun my/spacemacs-buffer//lord-lislon ()
  "Returns lord lislon news"
  (require 'org-agenda)
  )

(defun my/ediff-buffer-with-file (file-B &optional startup-hooks)
  "Run Ediff on a current buffer and other file"
  (interactive
   (list (ediff-read-file-name "File to compare"
                               (setq dir-B
                                     (if ediff-use-last-dir
                                         ediff-last-dir-B
                                       (file-name-directory buffer-file-name)))
                               (progn
                                 (ediff-add-to-history
                                  'file-name-history
                                  (ediff-abbreviate-file-name
                                   (expand-file-name
                                    (file-name-nondirectory buffer-file-name)
                                    dir-B)))
                                 (ediff-get-default-file-name buffer-file-name 1)))
         ))
  (ediff-files-internal (if (file-directory-p file-B)
                            (expand-file-name
                             (file-name-nondirectory file-A) file-B)
                          file-B)
                        buffer-file-name
                        nil ; file-C
                        startup-hooks
                        'ediff-files))

(setq custom-file (expand-file-name "~/Dropbox/dotfiles/spacemacs/emacs-custom.el"))
(load custom-file)



;; (cancel-timer my/timer)
