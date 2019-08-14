;;; init.el --- emacs init
;;; Commentary:
;;; Code:

;; Remember what I had open when I quit
;; (desktop-save-mode t)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(global-unset-key (kbd "C-z"))     ;; ctrl-z should *not* freeze a gui app
(global-unset-key (kbd "C-x C-z")) ;; why the heck would I ever want to suspend-frame

; overwrite selected text on insert
(delete-selection-mode 1)

(require 'package)

;;; nix takes care of package installation for me now
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
;; (setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

;; (unless (require 'use-package nil t)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package delight)

;; (setq use-package-always-ensure t)

(require 'ansi-view)

;; Define a new hook to be run after changing themes:
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; What I want to actually run after load-them:
(defun ansi-term-reset-color-vector ()
  "Attempt to unfuck `ansi-term' after changing themes."
  (setq ansi-term-color-vector
        [term term-color-black
              term-color-red
              term-color-green
              term-color-yellow
              term-color-blue
              term-color-magenta
              term-color-cyan
              term-color-white]))
;; Add my function to that new hook:
(add-hook 'after-load-theme-hook #'ansi-term-reset-color-vector)

(use-package spacemacs-common
  :config (load-theme 'spacemacs-light t))

;; Enable mouse input in terminals
(xterm-mouse-mode t)

(use-package bazel-mode
  :mode "BUILD\\'" "WORKSPACE\\'" "\\.bzl\\'")

(use-package company
  :diminish company-mode
  ;; Use company-mode in all buffers (more completion)
  :hook (after-init . global-company-mode))

(use-package company-posframe
  :diminish company-posframe-mode
  :config (company-posframe-mode 1))

(use-package company-terraform
  :config
  (company-terraform-init))

(diminish 'eldoc-mode)

(use-package arduino-mode)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package erlang)

(use-package flycheck
  :config
  ;; (setq flycheck-ghc-stack-use-nix t)
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pylint-executable "pylint")
  (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :after (flycheck)
  :config (flycheck-pos-tip-mode))

(use-package flycheck-color-mode-line
  :after (flycheck)
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-status-emoji
  :after (flycheck)
  :init
  (setq flycheck-status-emoji-indicator-finished-error ?💀)
  (setq flycheck-status-emoji-indicator-finished-ok ?👍)
  (setq flycheck-status-emoji-indicator-finished-warning ?👎)
  :config (flycheck-status-emoji-mode t))

(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package go-mode)

(use-package graphviz-dot-mode
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (setq graphviz-dot-view-command "dotty %s"))

(use-package haskell-mode
  :config (setq haskell-tags-on-save t))

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "bazel-.*/"))
(setq ido-auto-merge-work-directories-length -1)

(use-package flx-ido
  :config
  (flx-ido-mode 1))

(use-package highlight-indentation
  :hook
  (prog-mode . highlight-indentation-mode)
  (yaml-mode . highlight-indentation-mode)
  :diminish highlight-indentation-mode)

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package jsonnet-mode)

(use-package jq-mode
  :mode "\\.jq$")

;; (use-package kubernetes
;;   :commands (kubernetes-overview))

(defun benley/set-left-fringe-width ()
  (setq left-fringe-width 20))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (setq magit-completing-read-function #'magit-ido-completing-read)
  :hook (magit-mode . benley/set-left-fringe-width))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . gfm-mode)
  :config (setq markdown-command "pandoc")
  :hook (gfm-mode . turn-on-visual-line-mode))

(require 'mtail-mode)
(add-to-list 'auto-mode-alist (cons "\\.mtail$" #'mtail-mode))
(add-to-list 'auto-mode-alist (cons "\\.em$" #'mtail-mode))

(use-package nix-mode
  :config
  (setq nix-indent-function #'nix-indent-line)
  :mode
  ("\\.nix\\'" . #'nix-mode)
  ("\\.drv\\'" . #'nix-drv-mode))

(use-package nix-sandbox)
(require 'nix-sandbox)

(setq haskell-process-wrapper-function
      (lambda (args) (apply #'nix-shell-command (nix-current-sandbox) args)))

;; Remove after https://github.com/travisbhartwell/nix-emacs/pull/45 is merged:
(defun nix-shell-command (sandbox &rest args)
  "Assemble a command to be executed in SANDBOX from ARGS."
  (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox)
                            (mapconcat 'shell-quote-argument args " "))))

(setq flycheck-command-wrapper-function
      (lambda (cmd) (apply #'nix-shell-command (nix-current-sandbox) cmd)))

(setq flycheck-executable-find
      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

(use-package org)

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package org-journal
  :init
  ;; I think you have to set org-journal-dir before loading
  ;; org-journal for it to work correctly
  (setq org-journal-dir "~/benley@gmail.com/org/journal")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %B %e %Y")
  (setq org-journal-date-prefix "#+DATE: ")
  (setq org-journal-time-prefix "* ")
  (setq org-journal-time-format "%A, %B %e %Y %R %Z"))

;; (use-package org-make-toc
;;   ;; :init
;;   ;; (defalias 'second #'cadr)
;;   :hook (org-mode . org-make-toc-mode))

(use-package forge
  :after magit
  :init
  (when (< emacs-major-version 27)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
          ghub-use-workaround-for-emacs-bug nil))
  :ensure t)

(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode)
  (help-mode . form-feed-mode)
  :diminish form-feed-mode)



(use-package paredit)

(use-package powerline
  :init
  (setq powerline-default-separator 'slant)
  (setq powerline-gui-use-vcs-glyph t)
  (powerline-default-theme)
  :hook ((after-load-theme . powerline-reset)
         (after-init . powerline-reset)))

(use-package protobuf-mode)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(save-place-mode t)

(use-package smex
  ;; Put frequently-used commands at the front of ido completion list
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))

;; (use-package smooth-scrolling
;;   :init
;;   (setq smooth-scroll-margin 2)
;;   :config
;;   (smooth-scrolling-mode 1))

(use-package ido-grid-mode
  :init
  (setq ido-grid-mode-keys '(tab backtab up down left right C-n C-p C-s C-r))
  :config
  (ido-grid-mode +1))

;; from https://github.com/alphapapa/unpackaged.el#smerge-mode
(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package systemd)


(use-package terminal-here
  :init
  (setq terminal-here-terminal-command '("gnome-terminal"))
  (setq terminal-here-project-root-function #'projectile-project-root)
  :bind
  ("C-c C-S-t C-S-t" . terminal-here-launch)
  ("C-c C-S-t C-S-p" . terminal-here-project-launch))

(use-package treemacs
  :bind
  ("C-c t" . treemacs-select-window))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; (use-package vdiff
;;   :config
;;   (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

;; (use-package vdiff-magit
;;   :config
;;   (define-key magit-mode-map "e" #'vdiff-magit-dwim)
;;   (define-key magit-mode-map "E" #'vdiff-magit-popup)
;;   (setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
;;           '("vdiff dwim" #'vdiff-magit-dwim))
;;   (setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
;;           '("vdiff popup" #'vdiff-magit-popup))
;;   ;; This flag will default to using ediff for merges. vdiff-magit does not yet
;;   ;; support 3-way merges. Please see the docstring of this variable for more
;;   ;; information
;;   ;; (setq vdiff-magit-use-ediff-for-merges nil)

;;   ;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
;;   ;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
;;   ;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
;;   ;; when point is on an uncommitted hunk.  (setq vdiff-magit-dwim-show-on-hunks
;;   ;; nil)

;;   ;; Whether vdiff-magit-show-stash shows the state of the index.
;;   ;; (setq vdiff-magit-show-stash-with-index t)

;;   ;; Only use two buffers (working file and index) for vdiff-magit-stage
;;   ;; (setq vdiff-magit-stage-is-2way nil)
;;   )

(setq vterm-keymap-exceptions
      '("C-x" "M-x"))
(require 'vterm)

(use-package web-mode
  :init
  (defun custom-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))

  :mode
  "\\.html?\\'"
  "\\.css\\'"
  "\\.scss\\'"

  :hook
  (web-mode . custom-web-mode-hook))

;; (use-package weechat
;;   :config
;;    (setq weechat-modules '(weechat-button weechat-complete weechat-image weechat-notifications))
;;    (setq weechat-notification-mode t)
;;    (setq weechat-return-always-replace-input nil)
;;    (setq weechat-time-format "%H:%M"))

(use-package yaml-mode)

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") frame 'prepend)))

(--set-emoji-font nil)

(add-hook 'after-make-frame-functions #'--set-emoji-font)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)                  ;; show column position in modeline
(show-paren-mode 1)                     ;; highlight matching parens

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))        ;; ls doesn't have --dired on darwin

(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; unfortunately this makes a variety of things _extremely slow_
;; (add-hook 'text-mode-hook #'flyspell-buffer)

(setq flyspell-issue-message-flag nil)

;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist `(("." . "~/.emacs-backups")))

;; do not want
(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Save clipboard strings into kill ring before replacing them
(setq save-interprogram-paste-before-kill 1)

(setq browse-url-browser-function #'browse-url-chrome)
(setq inhibit-startup-screen t)
(setq require-final-newline t)
(setq mouse-yank-at-point t)  ;; middle-click paste where the cursor is, not
                              ;; wherever the mouse happens to be pointing at
                              ;; the time
(setq visible-bell t)   ;; STOP BEEPING >_<
(setq user-mail-address "benley@gmail.com")
(setq load-prefer-newer t)

(defun server-shutdown ()
  "Save buffers, quit, and shutdown the Emacs server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; keybindings for org mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c j") #'org-journal-new-entry)

;; Scrolling
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 2)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 2)))

;; Window movement
;; (global-set-key (kbd "s-h") #'windmove-left)
;; (global-set-key (kbd "s-j") #'windmove-down)
;; (global-set-key (kbd "s-k") #'windmove-up)
;; (global-set-key (kbd "s-l") #'windmove-right)

;; mouse back/fwd buttons
(global-set-key [mouse-8] #'previous-buffer)
(global-set-key [mouse-9] #'next-buffer)

;; FONTS
;; -----
(defun set-buffer-variable-pitch ()
  "Set variable-pitch font using `customize-face`.
Set the fonts to format correctly for specific modes.
Default face is fixed so we only need to have the exceptions."
  (interactive)
  (setq visual-fill-column-center-text t)
  (setq cursor-type '(bar . 3))
  (variable-pitch-mode t)
  (visual-line-mode t)
  (visual-fill-column-mode t)
  ;; (setq line-spacing 3)
  ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
  )

(defun benley/org-mode-setup ()
  (interactive)
  (setq fill-column 79)
  (visual-line-mode t))

(add-hook 'org-mode-hook #'benley/org-mode-setup)
(add-hook 'org-journal-mode-hook #'set-buffer-variable-pitch)

;; (add-hook 'org-mode-hook #'variable-pitch-mode)
;; (add-hook 'org-mode-hook #'visual-line-mode)

;; (add-hook 'org-mode-hook #'set-buffer-variable-pitch)

;; (add-hook 'markdown-mode-hook #'set-buffer-variable-pitch)
;; (add-hook 'Info-mode-hook #'set-buffer-variable-pitch)

;; (require 'ox-latex)
;; (setq org-latex-listings nil)
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

(defun rename-current-buffer-file ()
  "Rename the current buffer and the file it is visiting."
  ;; from https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs/37456354#37456354
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(require 'term)
(defun expose-global-binding-in-term (binding)
  "Expose BINDING from the global keymap in `term-mode'."
  (define-key term-raw-map binding
    (lookup-key (current-global-map) binding)))

;; Make M-x be same in ansi-term as everywhere else
(expose-global-binding-in-term (kbd "M-x"))

;; Same for C-c M-x to avoid confusion
(define-key term-raw-map (kbd "C-c M-x")
  (lookup-key (current-global-map) (kbd "M-x")))

;; ctrl-backspace doesn't do anything in normal terminals (there's no ascii
;; code for it), and keeping it bound to backward-kill-word was confusing me
;; endlessly.  Let's unmap that.
(define-key term-raw-map (kbd "<C-backspace>") #'term-send-backspace)

;; In shell-mode, make up and down arrows act more like a normal
;; (i.e. readline) shell prompt
(require 'shell)
(define-key shell-mode-map (kbd "<up>")   #'comint-previous-input)
(define-key shell-mode-map (kbd "<down>") #'comint-next-input)

(setq comint-prompt-read-only t)  ;; make various REPL prompts readonly
(setq comint-scroll-to-bottom-on-input 'this)  ;; scroll only _this_ window

;;; Enable some languages that I want to use with org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (jq . t)
   (ruby . t)
   (dot . t)))

(defun benley/org-confirm-babel-evaluate (lang body)
  "Don't prompt before evaluating yaml or dot blocks."
  (not (member lang '("yaml" "dot"))))

(setq org-confirm-babel-evaluate #'benley/org-confirm-babel-evaluate)

;; do-nothing execute function for yaml, so I can use yaml src blocks
;; as input to other code blocks
(defun org-babel-execute:yaml (body params) body)

;; make org bullets clickable, etc (part of org-mode)
(add-to-list 'org-modules 'org-mouse)

;; add github:... links to org mode (my local stuff)
(add-to-list 'org-modules 'org-github-links)

;; ox-gfm: Export to github-flavored markdown
(use-package ox-gfm)
(use-package ox-asciidoc)
(use-package ox-rst)
(use-package ox-ipynb)


(setq org-special-ctrl-a/e t)
(setq org-M-RET-may-split-line nil)
(setq org-agenda-start-on-weekday 0)
(setq org-catch-invisible-edits 'error)
(setq org-default-notes-file "~/benley@gmail.com/org/notes.org")
(setq org-directory "~/benley@gmail.com/org")
(setq org-ellipsis "⤵")
(setq org-footnote-define-inline t)
(setq org-goto-auto-isearch nil)
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-fontify-whole-heading-line t)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %LOCATION")
(setq org-hide-emphasis-markers t)

(setq org-agenda-files
      '("~/benley@gmail.com/org"
        "~/benley@gmail.com/org/journal"
        "~/benley@gmail.com/evernote_export"
        "~/.org-jira"))

;; I think org-jira covers this:
;; (add-to-list 'org-link-abbrev-alist '("jira" . "https://postmates.atlassian.net/browse/"))

(add-to-list 'org-link-abbrev-alist '("gmap" . "https://maps.google.com/maps?q=%s"))

;; https://emacs.stackexchange.com/questions/18404/can-i-display-org-mode-attachments-as-inline-images-in-my-document
(require 'org-attach)
(add-to-list 'org-link-abbrev-alist '("att" . org-attach-expand-link))

(setq org-capture-templates
      '(
        ("n" "Note" entry (file+headline "" "unfiled")
         "* NOTE %?\n  %U" :empty-lines 1)
        ("N" "Note+paste" entry (file+headline "" "unfiled")
         "* NOTE %?\n  %U\n  %c" :empty-lines 1)
        ("t" "Task" entry (file+headline "" "Tasks")
         "* TODO %?\n  %U\n  %a" :empty-lines 1)
        ("T" "Task+paste" entry (file+headline "" "Tasks")
         "* TODO %?\n  %U\n  %c" :empty-lines 1)
        ("e" "Event" entry (file+headline "" "Events")
         "* EVENT %?\n  %U" :empty-lines 1)
        )
      )

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)

(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq sh-learn-basic-offset 'usually)

(defun benley/prog-mode-hook ()
  "Setup stuff for `prog-mode' derivatives."
  (if window-system (hl-line-mode t))
  (setq-local show-trailing-whitespace t)
  (setq-local display-line-numbers t))

(add-hook 'prog-mode-hook #'benley/prog-mode-hook)

(defun benley/term-mode-hook ()
  "My `term-mode' hook."
  (goto-address-mode)  ;; Make URLs clickable
  (setq-local display-line-numbers nil))      ;; No line numbers in terminals

(add-hook 'term-mode-hook #'benley/term-mode-hook)

(defun benley/term-exec-hook ()
  "Try to make terminals work better with unicode I guess."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(add-hook 'term-exec-hook #'benley/term-exec-hook)

(use-package xterm-color)

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(defun benley/shell-mode-hook ()
  "Add xterm-color-filter to `comint-preoutput-filter-functions'.
This is what makes 256-color output work in shell-mode."
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))

(add-hook 'shell-mode-hook #'benley/shell-mode-hook)

(setq async-shell-command-buffer 'new-buffer)

(setq js-indent-level 2)

(setq woman-fill-frame t)

;; Set the X11 window title like "emacs: init.el (~/.emacs.d/init.el)"
(setq-default frame-title-format
              '(:eval
                (format "emacs: %s %s"
                        ;; (or (file-remote-p default-directory 'user)
                        ;;     user-real-login-name)
                        ;; (or (file-remote-p default-directory 'host)
                        ;;     system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-view-fit-page-to-window))

(use-package which-key
  :diminish
  which-key-mode
  :config
  (which-key-mode t)
  (which-key-setup-side-window-right-bottom))

(use-package ssh-config-mode
  :mode
  ("/\\.ssh/config\\'" . ssh-config-mode)
  ("/sshd?_config\\'" . ssh-config-mode)
  ("/known_hosts\\'" . ssh-known-hosts-mode)
  ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :hook
  (ssh-config-mode . turn-on-font-lock))  ; is this even necessary?

;; (use-package stumpwm-mode)

;; (use-package slime
;;   :init
;;   (setq slime-contribs '(slime-fancy)))

(setq calendar-latitude "42.3601")
(setq calendar-longitude "-71.0589")
(setq calendar-location-name "Boston, MA")

(defun benley/tabbar-buffer-groups-by-project ()
  "Group tabbar buffers by projectile project."
  (list
   (cond
    ((memq major-mode '(eshell-mode term-mode shell-mode))
     (if (projectile-project-p)
         (projectile-project-name)
       "Common"))
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs")
    ((memq major-mode '(fundamental-mode))
     "Emacs")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode org-journal-mode))
     "OrgMode")
    (t
     (if (projectile-project-p)
         (projectile-project-name)
       "Common")))))

(use-package tabbar
  :config
  (setq tabbar-buffer-groups-function 'benley/tabbar-buffer-groups-by-project)
  (tabbar-mode 1))

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/p/" "~/pm/"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  :delight
  '(:eval (concat " " (projectile-project-name))))

(use-package treemacs-projectile)

(use-package visual-fill-column
  ;; :hook
  ;; (visual-line-mode . visual-fill-column-mode)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package imenu-list
  :config
  (imenu-list-minor-mode t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-size 0.15)
  :bind
  ("C-'" . imenu-list-smart-toggle))

;; (server-start)
;; (load "~/.emacs.d/exwm.el")

(defun jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (call-process-region (point-min) (point-max)
                       jsonnet-command t t nil "fmt"
                       "--string-style" "l"
                       "--comment-style" "l"
                       "-"))

(use-package atomic-chrome
  :init
  (setq atomic-chrome-buffer-open-style 'frame)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("reddit\\.com" . markdown-mode)))
  (setq atomic-chrome-extention-type-list '(atomic-chrome))
  (setq atomic-chrome-buffer-frame-height 40)
  (setq atomic-chrome-buffer-frame-width 100)
  :config
  (atomic-chrome-start-server))

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

(load "~/.emacs.d/localonly.el")
(message "Finished with init.el")
(provide 'init)
;;; init.el ends here
