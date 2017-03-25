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
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d.local/layers")
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
     ;; (semantic :disabled-for emacs-lisp) ;; emacs 25 hang bug ;; problem counter: 1
     helm
     ;; ivy
     (auto-completion :disabled-for org markdown)
     better-defaults
     emacs-lisp
     common-lisp

     git
     github
     python
     html
     javascript
     yaml
     colors

     gnus
     sql
     dash ;; zeal help

     (markdown :variables markdown-live-preview-engine 'vmd)
     my-common
     my-org
     my-keys
     my-russian
     my-google-translate
     ; syntax-checking
     version-control
     ;;; csharp

     ;;; gtags
     latex
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(monokai-theme)

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(eyebrowse) ; Conflicts with org mode C-C C-w
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages nil)
  (when (eq system-type 'darwin)
    (add-to-list 'dotspacemacs-configuration-layers 'my-linux t))
  (when (eq system-type 'windows-nt)
    (add-to-list 'dotspacemacs-configuration-layers 'my-windows t))

  (when (file-exists-p "~/.spacemacs.d.local/layers/my-work")
    (add-to-list 'dotspacemacs-configuration-layers 'my-work t))

  (when (file-exists-p "~/Dropbox/confiles/common/emacs/layers")
    (add-to-list 'dotspacemacs-configuration-layer-path "~/Dropbox/confiles/common/emacs/layers" t)
    (add-to-list 'dotspacemacs-configuration-layers 'my-org-home t))
)


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
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes (my/select-first-if-day '(leuven monokai spacemacs-dark))
   dotspacemacs-themes '(monokai leuven spacemacs-dark)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil ;; this prevents inserting unbalanced closing )
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; maybe this helps from hangs.not testsed https://github.com/syl20bnr/spacemacs/issues/8462
   dotspacemacs-mode-line-unicode-symbols nil
   ediff-window-setup-function 'ediff-setup-windows-default
   configuration-layer-private-directory "~/.spacemacs.d"
   )
  (setq custom-file (expand-file-name "~/.emacs.d/.cache/custom.el")
        spacemacs-custom-file "~/.emacs.d/.cache/custom-spacemacs.el")
  ;; User initialization goes here
  (add-to-load-path "~/dotfiles/spacemacs/thirdparty")
  (add-to-load-path (expand-file-name "~/dotfiles/spacemacs/thirdparty/smsru/"))
  (when (getenv "https_proxy")
    (setq dotspacemacs-elpa-https nil))
)


(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; Shared undo of org files between computers
  (when (eq system-type 'gnu/linux)
    (setq shared-dir (expand-file-name "~/Dropbox/emacs-bookmarks/cache/undo")
          )
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
  (setq

   ;; basic
   vc-follow-symlinks t
   default-directory "~"
   evil-move-beyond-eol nil
   evil-escape-unordered-key-sequence t
   spaceline-org-clock-p t
   spaceline-always-show-segments t     ; always show clock
   x-select-enable-primary t            ; copy/paste from emacs to urxvt
   message-kill-buffer-query nil        ; do not ask confirmation of killing buffer
   ffap-newfile-prompt t                ; gf can create new files
   source-directory (concat user-home-directory "src-dormant/emacs/src")
   avy-case-fold-search nil             ; avy jump respect case
   spacemacs-paste-transient-state-add-bindings '(("p" evil-paste-pop "paste next")
                                                  ("P" evil-paste-pop-next "paste previous"))
   timer-max-repeats 0                  ; Do not repeat timer on suspen
   delete-by-moving-to-trash nil        ; Do not use trash when deleting files
   create-lockfiles nil                 ; get rid of .#filename.org (intented to prevent multiuser editing of file, but I always save files)
   evil-shift-round nil                 ; explanation https://youtu.be/HKF41ivkBb0?t=13m35s


   yas-wrap-around-region nil           ; I set to nil to prevent duplication of region when using surround snip
   ;; Dired - directories first

   ;; dired-listing-switches "-aBhl  --group-directories-first"

   ;; Man in same window
   Man-notify-method 'pushy

   ;; ftp
   ange-ftp-try-passive-mode t

   ;; backup

   ;; make-backup-files Switched off because of error backup-extract-version: Args out of range: "!home!ele!Dropbox!emacs-resources!gnus!archive!Отправленные.~1~", 73
   make-backup-files nil
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
   evil-ex-substitute-global t

   ;; zsh shell
   ;; comint-input-ring-size 100000
   ;; comint-input-ring-file-name "~/.zsh_history"
   ;;                                      ; Ignore timestamps in history file.  Assumes that zsh
   ;;                                      ; EXTENDED_HISTORY option is in use.
   ;; comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);"

   ;; ispell-personal-dictionary "~/Dropbox/emacs-resources/ispell-dictionary"
   abbrev-file-name "~/Dropbox/emacs-resources/abbrev_defs.el"
   ;; spell-checking-enable-auto-dictionary t
   spell-checking-enable-by-default nil ; Disabled because it freezes emacs 25.1 when commenting XML region with ending line

   ;; big undo
   undo-limit (* 10 1000 1000)                    ; 10MB per file
   undo-strong-limit (* 100 1000 1000)            ; 100MB max

   ;; org-mode timestamps in english
   system-time-locale "C"

   ;; Git fullscreen
   git-magit-status-fullscreen t

   ;; Tea time sound
   timer-sound "~/Dropbox/dotfiles/spacemacs/sounds/tea-bell.wav"

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

   spacemacs-useful-buffers-regexp '("\\*SQL*"
                                     "\\*Man*"
                                     "\\*unset mail*"
                                     "\\*Article*"
                                     "\\*Dir\\*"
                                     "\\*info\\*"
                                     "\\*sos\\*"
                                     "\\*REST\\*"
                                     "\\*new snippet\\*"
                                     "\\*Help\\*")
   )

  ;; prevent overwire of clipboard when selection
  (fset 'evil-visual-update-x-selection 'ignore)

  (make-directory (concat spacemacs-cache-directory "undo") t)

  ;; ------------------------------------------------------------------------------
  ;; DEBUG recentf
  ;; ------------------------------------------------------------------------------
  ;; (setq debug-on-quit t)

  ;; (with-eval-after-load "recent"
  ;;   (defun recentf-track-opened-file ()
  ;;     "Insert the name of the file just opened or written into the recent list."

  ;;     (and buffer-file-name (message "recentf track file: %s" buffer-file-name)
  ;;          (recentf-add-file buffer-file-name))
  ;;     ;; Must return nil because it is run from `write-file-functions'.
  ;;     nil))

  ;; (add-hook 'find-file-hook (lambda () (message "find file hook called %s" buffer-file-name)))

  ;; Why do we need this shit?
  ;; (with-eval-after-load "org"
  ;;   (mapcar 'funcall org-store-link-functions))

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

  ;; startundo
  ;; Custom bookmakrs path
  ;; in emacs 24 system-name is FQDN and in emacs 25 it is a hostname
  (setq bookmark-default-file "~/.spacemacs.d.local/bookmarks.el")

  (put 'set-goal-column 'disabled nil)  ;; Allow C-x C-n goal column for csv


;;   ;; C-h - updirectory in dired
;;   (evil-define-key 'normal dired-mode-map (kbd "C-S-h") 'dired-up-directory)

;;   ;; Fix RET key in profiler (not works)
;;   ;; (evil-define-key 'normal profiler-report-mode-map (kbd "j") 'profiler-report-toggle-entry)


  ;; auto-save all files on lost focus
  (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
  (global-set-key (kbd "C-c t") 'org-timer-set-timer)
  (global-unset-key (kbd "C-h h")) ;; Disable hello file, because it hangs

  ;; autoscroll messages
  ;; commented because i can't copy line with yy
  ;; TODO: Add check that cursor is not in current window
  ;; (defadvice message (after message-tail activate)
  ;;   "goto point max after a message"
  ;;   (with-current-buffer "*Messages*"
  ;;     (goto-char (point-max))
  ;;     (walk-windows (lambda (window)
  ;;                     (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
  ;;                         (set-window-point window (point-max))))
  ;;                   nil
  ;;                   t)))



  ; Auto set lisp-interaction-mode in *scratch*
  ;; @deprec Now it is dotspacemacs-scratch-mode 'lisp-interaction-mode
  ;; (defun my/scratch-interactive-mode ()
  ;;   (when (equal "*scratch*" (buffer-name))
  ;;     (lisp-interaction-mode)
  ;;     (remove-hook 'window-configuration-change-hook 'my/scratch-interactive-mode))
  ;;   )
  ;; (add-hook 'window-configuration-change-hook 'my/scratch-interactive-mode)

  ;; (add-hook 'calc-mode-hook 'calc-algebraic-entry)


  ;; Auto github favore mode when editing markdown
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("vimrc\\'" . vimrc-mode))

  ;; Shell mode for ~/bin
  (add-to-list 'auto-mode-alist '("rc\\'" . conf-mode))
  (add-to-list 'auto-mode-alist `(,(concat user-home-directory "bin/.+") . shell-script-mode))
  (add-to-list 'auto-mode-alist '("dictionary\\.txt\\'" . google-translate-interactive-mode))
  (add-to-list 'auto-mode-alist '("\\.desktop\\'" . desktop-entry-mode))

  (define-derived-mode desktop-entry-mode conf-unix-mode "Desktop" "Desktop entry")



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


  ;; Private config for this host
  (when (file-exists-p "~/local.el")
    (load "~/local.el"))

  ;; Private config for all my hosts
  (when (file-exists-p "~/confiles/common/emacs/semi-local.el")
    (load "~/confiles/common/emacs/semi-local.el"))

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


  ;; make this buffers available for switching via SPC TAB

  (dotspacemacs/custom-keys)
  (dotspacemacs/hooks)

  ;; (use-package web-mode
  ;;   :defer t
  ;;   :init
  ;;   (setq web-mode-extra-snippets
  ;;         '(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
  ;;           ("php" . (("dowhile" . "<?php do { ?>\n\n<?php } while (|); ?>")
  ;;                     ("debug" . "<?php error_log(__LINE__); ?>")))
  ;;           )))

  ;; Integrate system clipboard with emacs kill ring
  ;; disable reson: It doesn't work with russian text in clipboard
  ;; (use-package clipmon
  ;;   :config (clipmon-mode-start))
  (define-key process-menu-mode-map (kbd "C-k") 'joaot/delete-process-at-point)
  )

(defun dotspacemacs/custom-keys ()
  "Here go my keys customization."

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
  (global-set-key (kbd "C-c t") 'my/org-timer-set-timer)

  ;; Make C-j work with evil in *scratch*
  (evil-define-key 'normal lisp-interaction-mode-map (kbd "C-j") 'my/eval-print-sexp-line)


  (evil-leader/set-key
    "fd" 'my/ediff-buffer-with-file
    "oJ" 'my/java-text
    ;; "os" 'yas-visit-snippet-file
    "os" (lambda () (interactive) (error "Use SPC i s to list snippets"))
    ;; "oi" (lambda () (interactive) (error "Use SPC i s to insert yasnippet"))
    "oS" 'yas-new-snippet
    ;; "oS" (lambda () (interactive) (error "Use SPC i S c to create yasnippet"))
    "az" 'shell
    "as" 'eshell
    "hs" 'sos                           ; Stackoverflow help
    ;; "os" 'just-one-space
    "swy" 'engine/search-youtube
    )

  (spacemacs/set-leader-keys-for-major-mode 'sh-mode
    "rv" 'my/sh-extract-variable)

  ;; documentation for snippet mode
  (spacemacs/set-leader-keys-for-major-mode 'snippet-mode
    "h" (lambda () (interactive)
                        (browse-url "http://joaotavora.github.io/yasnippet/snippet-development.html")))

  (spacemacs/set-leader-keys-for-major-mode 'gfm-mode
    "h" (lambda () (interactive) (browse-url "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet")))

  (spacemacs/set-leader-keys-for-major-mode 'desktop-mode
    "i" 'my/install-desktop-file)

  ;; connect to mysql via SPC m c
  ;; (spacemacs/set-leader-keys-for-major-mode 'sql-mode "c" 'my/sql-connect-preset)
)

(defun dotspacemacs/hooks ()
  "Here go my keys customization."

  ;; Beyond eol makes ~SPC m e e~ usable
  (add-hook 'lisp-mode-hook ((lambda () (setq-local evil-move-beyond-eol t))))
  (add-hook 'auto-save-hook 'auto-save-all-files)
  (add-hook 'shell-mode-hook 'my-shell-mode-hook)
  )


;; TODO implement this
 ; swap 1 and 2
(setq custom-file (expand-file-name "~/.emacs.d/.cache/custom.el")
      spacemacs-custom-file "~/.emacs.d/.cache/custom-spacemacs.el")
(if (file-exists-p custom-file)
    (load custom-file))
(load "~/Dropbox/dotfiles/spacemacs/funcs.el")
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)
