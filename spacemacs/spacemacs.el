;;; .spacemacs --- My personal configuration file for spacemacs
;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
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
     auto-completion
     better-defaults
     emacs-lisp
     common-lisp
     ;; browser-edit
     chrome
     html
     lua
     shell-scripts
     git
     github
     playground
     colors
     java
     javascript
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
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/user-init ()
  (setq-default

   ;; basic
   vc-follow-symlinks t
   default-direcotry "~"
   evil-move-beyond-eol nil
   evil-escape-unordered-key-sequence t
   spaceline-org-clock-p t

   ;; backup
   make-backup-files t
   keep-new-versions 10
   keep-old-versions 2
   backup-by-copying t
   delete-old-versions t
   version-control t       ;; Always use version numbers in filenames
   backup-directory-alist '(("" . "~/ . emacs.d/.cache/backup-per-save"))

   ;; workaround: neotree is painfully slow
   neo-vc-integration nil

   ;; Autocompletion
   auto-completion-complete-with-key-sequence "jk"
   auto-completion-enable-snippets-in-popup t
   auto-completion-enable-help-tooltip t

   ;; IRC
   erc-autojoin-channels-alist
   '(("1\\.0\\.0" "#syl20bnr/spacemacs") ; Gitter
     ("freenode\\.net" "#emacs"))
   erc-timestamp-format-left "\n%A %B %e, %Y\n\n"
   erc-timestamp-format-right "%H:%M"
   erc-timestamp-right-column 80
   erc-prompt-foc-nickserv-password nil
   erc-image-inline-rescale 300
   erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
   erc-foolish-content
   '("\\[Github\\].* starred"
     "\\[Github\\].* forked"
     "\\[Github\\].* synchronize a Pull Request"
     "\\[Github\\].* labeled an issue in"
     "\\[Github\\].* unlabeled an issue in")

   ;; Misc
   google-translate-default-source-language "en"
   google-translate-default-target-language "ru"

   ;; org-mode timestamps in english
   system-time-locale "C"

   ;; ANSI Lisp
   inferior-lisp-program "clisp"
  )
  )

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents bookmarks)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         wombat
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
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
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
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
   ;; Use to disable fullscreen animations in OSX."
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
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
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
   )
  ;; User initialization goes here
  )
;; ============================================================

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; Swap C-j and C-x
  (define-key key-translation-map (kbd "C-j") (kbd "C-x"))
  (define-key key-translation-map (kbd "C-x") (kbd "C-j"))
  ;; (setq spaceline-org-clock-p t)

  (setq  spaceline-org-clock-p t)
  ;; Persistent undo
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; M-\ to escape from insert mode (from russian language)
  (define-key evil-hybrid-state-map (kbd "M-\\") 'evil-escape)

  ;; Vim move right or left
  (define-key evil-normal-state-map "L" 'evil-end-of-line)
  (define-key evil-normal-state-map "H" 'spacemacs/smart-move-beginning-of-line)

  ;; Emacs yank C-y in insert mode

  ;; (define-key evil-hybrid-state-map "\C-y" 'yank)

  ;; ;; Use tab in help mode for next button
  (define-key help-mode-map (kbd "<Tab>") 'forward-button)

  ;; (with-eval-after-load 'eww-mode
  ;;   (define-key eww-mode-map (kbd "<Tab>") 'forward-button))
  (add-hook 'eww-mode 'evil-insert-state)

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

  ;; ;; Custom bookmakrs path
  ;; (setq bookmark-default-file "~/.emacs.d/private/bookmarks.el")


  (define-generic-mode 'vimrc-generic-mode nil nil
    '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
       (0 font-lock-warning-face))
      ("\\(^\\|[\t ]\\)\\(\".*\\)$"
       (2 font-lock-comment-face))
      ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
       (0 font-lock-string-face)))
    '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
    '((lambda ()
        (modify-syntax-entry ?\" ".")))
    "Generic mode for Vim configuration files.")

   ;; Disable hello file, because it hangs
  (global-unset-key (kbd "C-h h"))

  (defun my/keys-help-sheet (args)
    "Shows cheatsheet with hotkeys from todo.org"
    (interactive "P")
    (split-window-right-and-focus)
    (unless (get-buffer "Keys")
        (find-file "~/org/todo.org")
        (let ((buffer (make-indirect-buffer (current-buffer) "Keys")))
          (switch-to-buffer buffer)
          (org-mode)
          (spacemacs/toggle-current-window-dedication)
        ))
    (switch-to-buffer (get-buffer "Keys"))
    (widen)
    (goto-char (point-min))
    (org-tags-sparse-tree nil "keys")
    (next-match)
    (org-narrow-to-subtree)
    (org-cycle 2)
    )


  ;; Fix tab key
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)

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

  (defvar my/org-mobile-sync-timer nil)

  (defvar my/org-mobile-sync-secs (* 60 20))

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
          (run-with-idle-timer my/org-mobile-sync-secs t
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
          (beginning-of-buffer)
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
  (use-package google-translate
    :commands (my/google-translate-repl)
    :config
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
          (define-key evil-normal-state-map (kbd "RET") 'my/translate-word-and-next-line))

       (defun my/google-translate-repl ()
          (interactive)
          (let ((buffer (get-buffer-create "Google Translate REPL")))
              (switch-to-buffer buffer)
              (google-translate-interactive-mode)
              (evil-insert-state)
              (goto-char (buffer-end 1))
              ))
    )

    (defun lsn-insert-line-and-paste (count)
      "Moves to new line and paste text"
        (interactive "P")
        (move-end-of-line nil)
        (newline)
        (evil-paste-after count))

  (evil-leader/set-key "x g i" 'my/google-translate-repl)
  ;; SPC o k - Show cheatsheet with hotkeys
  (evil-leader/set-key "ok" 'my/keys-help-sheet)
  (evil-leader/set-key "os" 'yas-visit-snippet-file)
  (evil-leader/set-key "oS" 'yas-new-snippet)
  (define-key evil-normal-state-map (kbd "gp") 'lsn-insert-line-and-paste)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
  
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
  ;; on Linux emacs daemon it complains on error
  ;; (server-start)

  ; Auto set lisp-interaction-mode in *scratch*
  (defun my/scratch-interactive-mode ()
    (when (equal "*scratch*" (buffer-name))
      (lisp-interaction-mode)
      (remove-hook 'window-configuration-change-hook 'my/scratch-interactive-mode))
    )
  (add-hook 'window-configuration-change-hook 'my/scratch-interactive-mode)
  ;; Beyond eol makes ~SPC m e e~ usable
  (add-hook 'lisp-mode-hook ((lambda () (setq-local evil-move-beyond-eol t))))

    ;; Using chrome as default browser
  (if (eq system-type 'gnu/linux)
        (setq browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "chromium"))

    ;; Auto github favore mode when editing markdown
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

    ;; Load local
    (when (file-exists-p "~/local.el")
      (load "~/local.el"))

    (require 'org)
    ;; End of private config
    )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

