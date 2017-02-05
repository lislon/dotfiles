(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(evil-disable-insert-state-bindings t)
 '(evil-want-Y-yank-to-eol t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (hexrgb font-lock+ smartparens highlight helm helm-core projectile with-editor company ivy xterm-color eyebrowse zeal-at-point yapfify yaml-mode xelb wttrin ws-butler window-numbering which-key web-mode web-beautify w3m volatile-highlights vmd-mode vimrc-mode vimgolf vi-tilde-fringe uuidgen use-package undohist toc-org tea-time tagedit systemd swiper stickyfunc-enhance srefactor sql-indent spacemacs-theme spaceline sos smeargle slime-company slim-mode scss-mode sass-mode restclient restart-emacs refine ranger rainbow-mode rainbow-identifiers rainbow-delimiters quickrun quelpa pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el paradox palette orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets openwith open-junk-file noflet nginx-mode neotree mwim move-text monokai-theme mmm-mode markdown-toc magit-gitflow magit-gh-pulls lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode js2-refactor js-doc insert-shebang info+ indent-guide impatient-mode ido-vertical-mode hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-systemd helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flyspell-correct-helm flycheck-pos-tip flx-ido fish-mode fill-column-indicator fasd fancy-battery f3 expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks engine-mode emoji-cheat-sheet-plus emmet-mode elisp-slime-nav dumb-jump drupal-mode dockerfile-mode docker disaster diff-hl define-word cython-mode company-web company-tern company-statistics company-shell company-emoji company-emacs-eclim company-c-headers company-auctex company-anaconda common-lisp-snippets column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clipmon clean-aindent-mode clang-format calfw bbdb auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk all-the-icons-dired ahk-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((httpd-root . ~/src/nodejs/lfs-dashboard)
     (org-list-allow-alphabetical . t)
     (company-clang-arguments "-I/usr/include")
     (eval progn
           (defun my/rc-lua-copy-to-test nil
             (if
                 (and
                  (string= "rc.lua"
                           (buffer-name))
                  (file-exists-p "rc.lua.test"))
                 (copy-file "rc.lua" "rc.lua.test" t)))
           (add-hook
            (quote after-save-hook)
            (function my/rc-lua-copy-to-test)
            nil t)
           (defun my/awesomewm-documentation nil "Browse awesomewm documentation API"
                  (interactive)
                  (browse-url "https://awesome.naquadah.org/doc/api/"))
           (spacemacs/set-leader-keys-for-major-mode
             (quote lua-mode)
             "l"
             (quote my/awesomewm-documentation)))
     (eval progn
           (defun my/rc-lua-copy-to-test nil
             (if
                 (and
                  (string= "rc.lua"
                           (buffer-name))
                  (file-exists-p "rc.lua.test"))
                 (copy-file "rc.lua" "rc.lua.test" t)))
           (add-hook
            (quote after-save-hook)
            (function my/rc-lua-copy-to-test)
            nil t)))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(smtpmail-smtp-server "smtp.mail.ru")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
