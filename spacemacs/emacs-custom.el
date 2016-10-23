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
    (engine-mode phpunit phpcbf php-extras php-auto-yasnippets drupal-mode php-mode vmd-mode mmm-mode markdown-toc markdown-mode gh-md flyspell-correct slime-company insert-shebang hide-comnt helm-purpose window-purpose imenu-list swiper ivy refine py-isort org-download mwim hydra iedit dumb-jump company-shell cmake-mode clang-format bbdb auctex-latexmk auctex eclim smartparens undo-tree ht yasnippet helm helm-core projectile magit magit-popup git-commit with-editor simple-httpd s ranger fasd grizzl yaml-mode pug-mode dash async flyspell-correct-helm wgrep ivy-hydra flyspell-correct-ivy counsel-projectile dockerfile-mode docker tablist docker-tramp palette hexrgb gnus-desktop-notify zenburn-theme monokai-theme solarized-theme helm-pydoc helm-gtags helm-gitignore helm-flyspell helm-css-scss helm-company helm-c-yasnippet google-translate undohist xkcd web-mode toc-org tagedit stickyfunc-enhance srefactor smex smeargle slime slim-mode scss-mode sass-mode rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode pip-requirements orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets omnisharp noflet magit-gitflow magit-gh-pulls lua-mode livid-mode live-py-mode less-css-mode json-mode js2-refactor js2-mode js-doc jade-mode hy-mode htmlize haml-mode gnuplot gmail-message-mode gitignore-mode github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter gist ggtags flycheck-pos-tip flycheck fish-mode evil-magit erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus emmet-mode emacs-eclim edit-server diff-hl cython-mode counsel company-web company-tern company-statistics company-quickhelp company-emoji company-anaconda company common-lisp-snippets coffee-mode auto-yasnippet auto-dictionary anaconda-mode ac-ispell xelb ws-butler window-numbering which-key web-beautify volatile-highlights vi-tilde-fringe use-package tea-time systemd spacemacs-theme spaceline smooth-scrolling restart-emacs rainbow-delimiters quickrun quelpa popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag golden-ratio flx-ido fill-column-indicator fancy-battery f expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((org-list-allow-alphabetical . t)
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
