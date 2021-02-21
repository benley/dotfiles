;;; init.el --- emacs init   -*- lexical-binding: t; -*-
;;; Commentary:

;; https://blog.d46.us/advanced-emacs-startup/

;;; Code:

;; (setq debug-on-error t)

;; Workaround obnoxious TLS thing in Debian stable
(require 'gnutls)
(when (version< emacs-version "27")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Crank up GC parameters during startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; After startup, set more conservative GC options
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1)))

;; During startup, clear file-name-handler-alist (we'll put it back later)
(defvar benley--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; After startup, restore file-name-handler-alist
(add-hook 'emacs-startup-hook
          (lambda () (setq file-name-handler-alist benley--file-name-handler-alist)))



(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(global-unset-key (kbd "C-z"))     ;; ctrl-z should *not* freeze a gui app
(global-unset-key (kbd "C-x C-z")) ;; why the heck would I ever want to suspend-frame

;;; nix takes care of package installation for me now
;; (require 'package)
;; (setq package-archives
;;       '(("gnu"   . "https://elpa.gnu.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")
;;         ("org"   . "https://orgmode.org/elpa/")))
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize) ;; seemingly necessary for flycheck-verify-setup to work

;; this has to be set before loading use-package in order to work
(setq use-package-enable-imenu-support t)

;; (unless (require 'use-package nil t)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish)
(use-package delight)

;; (setq use-package-always-ensure t)

(use-package scroll-bar
  :config
  (customize-set-variable 'scroll-bar-mode nil))

(use-package menu-bar
  :config
  (customize-set-variable 'menu-bar-mode nil))

(use-package tool-bar
  :config
  (customize-set-variable 'tool-bar-mode nil))

(use-package delsel
  :config
  ;; overwrite selected text on insert
  (customize-set-variable 'delete-selection-mode t))

;; (require 'ansi-view)


;; Define a new hook to be run after changing themes:
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(advice-add 'load-theme :after
            (lambda (&rest r)
              "Run `after-load-theme-hook'."
              (run-hooks 'after-load-theme-hook)))

;; What I want to actually run after load-them:
(defun ansi-term-reset-color-vector (&rest r)
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


;; Define a new hook to be run after creating a new frame
;; (defvar server-create-window-system-frame-hook nil
;;   ;; TODO: emacs 27 has server-after-make-frame-hook which might replace this
;;   "Hook run after Emacs server creates a GUI frame.")

;; (advice-add 'server-create-window-system-frame :after
;;             (lambda (&rest r)
;;               "Run `server-create-window-system-frame-hook'."
;;               (run-hooks 'server-create-window-system-frame-hook)))


;;; THEMES

(use-package all-the-icons)

(use-package centaur-tabs
  :after all-the-icons
  :demand
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :preface
  (defun benley/centaur-tabs-hide-tab-wrapper (orig-fn &rest args)
    "Wrapper for the default centaur-tabs-hide-tab function."
    (let ((name (format "%s" (car args))))
      (or (string-prefix-p "*Ilist*" name)
          (string-prefix-p "*Completions*" name)
          (string-equal "TAGS" name)
          (apply orig-fn args))))
  :config
  (customize-set-variable 'centaur-tabs-set-icons t)
  (customize-set-variable 'centaur-tabs-style "bar")
  (customize-set-variable 'centaur-tabs-set-bar 'over)
  (customize-set-variable 'centaur-tabs-height 45)
  (customize-set-variable 'centaur-tabs-set-modified-marker t)
  (customize-set-variable 'centaur-tabs-mode t)

  (centaur-tabs-group-by-projectile-project)
  (advice-add 'centaur-tabs-hide-tab :around #'benley/centaur-tabs-hide-tab-wrapper)
  :hook
  (imenu-list-major-mode . centaur-tabs-local-mode)
  (after-load-theme . centaur-tabs-headline-match))

(use-package solaire-mode
  :ensure t
  ;; I'm not sure if this first set of hooks is necessary, or if
  ;; solaire-global-mode takes care of it
  ;; :hook
  ;; ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  ;; (minibuffer-setup . solaire-mode-in-minibuffer)
  ;; (after-load-theme . solaire-global-mode)
  :config
  (setq solaire-mode-auto-swap-bg t)
  (solaire-global-mode t))

(use-package doom-themes
  :ensure t
  :config
  (require 'doom-themes-ext-treemacs)
  (require 'doom-themes-ext-org)
  (customize-set-variable 'doom-themes-treemacs-enable-variable-pitch nil)
  (customize-set-variable 'doom-themes-treemacs-theme "doom-colors")
  ;; (customize-set-variable 'custom-enabled-themes '(doom-palenight))
  )

(setq benley--theme-loaded nil)
(setq benley--theme 'doom-palenight)

(load-theme benley--theme t)

(defun benley--load-theme ()
  "Load my chosen them, but only do it once.

When using server mode, we want to (re)load the theme exactly
once while creating the first frame, so solaire-mode can do its
thing properly."
  (when (null benley--theme-loaded)
    (load-theme benley--theme t)
    (message "LOADED MY THEME HRURR")
    (setq benley--theme-loaded t)))

(add-hook 'server-after-make-frame-hook #'benley--load-theme)

(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :config
  (customize-set-variable 'doom-modeline-icon t)
  (customize-set-variable 'doom-modeline-mode t))

(use-package xt-mouse
  :config
  (customize-set-variable 'xterm-mouse-mode t))

(use-package bazel-mode
  :mode
  ((rx ?/ (or "BUILD" "BUILD.bazel") eos) . #'bazel-build-mode)
  ((rx ?/ (or "WORKSPACE" "WORKSPACE.bazel") eos) . #'bazel-workspace-mode)
  ((rx ?/ (+ nonl) (or ".starlark" ".bzl") eos) . #'bazel-starlark-mode)
  ((rx ?/ "Tiltfile" eos) . #'bazel-starlark-mode)
  ((rx 47 (or "bazel.bazelrc" ".bazelrc") eos) . #'bazelrc-mode))

(use-package company
  :diminish company-mode
  :defer t
  :config
  (customize-set-variable 'company-tooltip-align-annotations t)
  :hook
  (after-init . global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (customize-set-variable 'company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package company-nixos-options
  ;; :defer 5
  :after nix-mode
  :config
  ;; patch in https://github.com/travisbhartwell/nix-emacs/pull/46
  ;; which is apparently never going to be merged
  (defun company-nixos--in-nix-context-p ()
    (or (derived-mode-p 'nix-mode 'nix-repl-mode)
        (let ((file-name (buffer-file-name (current-buffer))))
          (and file-name (equal "nix" (file-name-extension file-name))))))
  (add-to-list 'company-backends 'company-nixos-options))

(use-package terraform-mode
  :mode "\\.tf\\(vars\\)?\\'")

(use-package company-terraform
  :after terraform-mode
  :config
  (add-to-list 'company-backends 'company-terraform))

(use-package eldoc
  :diminish eldoc-mode)

(use-package arduino-mode
  :mode "\\.pde\\'" "\\.ino\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\(?:\\..*\\)?\\'")

(use-package flycheck
  :custom
  ;; (flycheck-ghc-stack-use-nix t)
  (flycheck-python-flake8-executable "flake8")
  (flycheck-python-pylint-executable "pylint")
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

;;;; annoying with lsp-mode's config
;; (use-package flycheck-pos-tip
;;   :after flycheck
;;   :hook
;;   (flycheck-mode . flycheck-pos-tip-mode))

;;;; Not needed with doom-modeline
;; (use-package flycheck-color-mode-line
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-color-mode-line-mode))

;;;; Not needed with doom-modeline
;; (use-package flycheck-status-emoji
;;   :after flycheck
;;   :custom
;;   (flycheck-status-emoji-indicator-finished-error ?💀)
;;   (flycheck-status-emoji-indicator-finished-ok ?👍)
;;   (flycheck-status-emoji-indicator-finished-warning ?👎)
;;   :hook
;;   (flycheck-mode . flycheck-status-emoji-mode))

(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode +1))

(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)

(use-package go-mode
  :mode
  ("\\.go\\'" . go-mode)
  ("go\\.mod\\'" . go-dot-mod-mode))

(use-package graphviz-dot-mode
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  :custom
  (graphviz-dot-view-command "dotty %s"))

(use-package haskell-mode
  :custom
  (haskell-tags-on-save t)
  (haskell-interactive-popup-errors nil)
  (haskell-process-show-overlays nil)
  (haskell-process-use-presentation-mode nil)
  :config
  ;; (require 'w3m-haddock)
  (load "pragmatapro-prettify-symbols-v0.828")
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . prettify-symbols-mode)
  (prog-mode . prettify-hook)
  )

(use-package ido-completing-read+  ;; used by magit
  :ensure t)

(use-package ido
  :config
  (customize-set-variable 'ido-mode 'both)
  (customize-set-variable 'ido-everywhere t)
  (customize-set-variable 'ido-enable-flex-matching t)
  (customize-set-variable 'ido-use-faces nil)
  (customize-set-variable 'ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "bazel-.*/"))
  (customize-set-variable 'ido-auto-merge-work-directories-length -1))

(use-package flx-ido
  :config
  (customize-set-variable 'flx-ido-mode 1))

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :config
  (customize-set-variable 'highlight-indent-guides-responsive 'stack)
  (customize-set-variable 'highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (yaml-mode . highlight-indent-guides-mode))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package json-mode
  :custom
  (json-reformat:indent-width 2))

(use-package jsonnet-mode
  :mode "\\.jsonnet\\'" "\\.libsonnet\\'"
  :config
  (defun jsonnet-reformat-buffer ()
    "Reformat entire buffer using the Jsonnet format utility."
    (interactive)
    (call-process-region (point-min) (point-max)
                         jsonnet-fmt-command t t nil
                         "--string-style" "l"
                         "--comment-style" "l"
                         "--no-sort-imports"
                         "-")))

(use-package jq-mode
  :mode "\\.jq\\'")

;; (use-package lilypond-mode
;;   :ensure t
;;   :mode (("\\.ly\\'" . LilyPond-mode)
;;          ("\\.ily\\'" . LilyPond-mode)))

(use-package magit
  :preface
  (defun benley/set-left-fringe-width ()
    (setq left-fringe-width 20))
  :bind ("C-x g" . magit-status)
  :custom
  (magit-branch-prefer-remote-upstream '("master" "main"))

  :config
  (customize-set-variable 'magit-completing-read-function #'magit-ido-completing-read)
  (customize-set-variable 'magit-section-visibility-indicator
			  '(magit-fringe-bitmap-bold> . magit-fringe-bitmap-boldv))
  :hook (magit-mode . benley/set-left-fringe-width))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (customize-set-variable 'markdown-command "pandoc")
  :hook (gfm-mode . turn-on-visual-line-mode))

(use-package nix-mode
  :defer t
  :config
  (customize-set-variable 'nix-indent-function #'nix-indent-line)
  :mode
  ("\\.nix\\'" . nix-mode)
  ("\\.drv\\'" . nix-drv-mode))

(use-package nix-sandbox
  :config
  ;; Should be obsolete after https://github.com/travisbhartwell/nix-emacs/pull/45 is merged:
  (defun benley/nix-shell-command (sandbox &rest args)
    "Assemble a command to be executed in SANDBOX from ARGS."
    (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox)
                              (mapconcat 'shell-quote-argument args " "))))
  (defalias 'nix-shell-command #'benley/nix-shell-command)

  (defun benley--set-python-interpreter ()
    (setq-local python-shell-interpreter
                ;; (format "nix-shell %s --run python" (nix-current-sandbox))))
                (nix-executable-find (nix-current-sandbox) "python")))
  (add-hook 'python-mode-hook #'benley--set-python-interpreter)

  :config
  (customize-set-variable 'haskell-process-wrapper-function
			  (lambda (cmd) (apply #'nix-shell-command (nix-current-sandbox) cmd)))
  (customize-set-variable 'flycheck-command-wrapper-function
			  (lambda (cmd) (apply #'nix-shell-command (nix-current-sandbox) cmd)))
  (customize-set-variable 'flycheck-executable-find
			  (lambda (cmd) (nix-executable-find       (nix-current-sandbox) cmd))))

(use-package org
  :init
  (defun benley/org-mode-setup ()
    (interactive)
    (setq fill-column 79))

  :hook
  (org-mode . visual-line-mode)
  (org-mode . hl-line-mode)
  (org-mode . benley/org-mode-setup)

  :custom
  (org-attach-store-link-p 'attached)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-image-actual-width nil)
  (org-special-ctrl-a/e t)
  (org-M-RET-may-split-line nil)
  (org-agenda-start-on-weekday 0)
  (org-catch-invisible-edits 'error)
  (org-default-notes-file "~/benley@gmail.com/org/notes.org")
  (org-directory "~/benley@gmail.com/org")
  (org-ellipsis "⤵")
  (org-footnote-define-inline t)
  (org-goto-auto-isearch nil)
  (org-log-done 'time)
  (org-startup-indented t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %LOCATION")
  (org-hide-emphasis-markers t)
  (org-todo-keywords '((sequence "TODO" "WIP" "|" "DONE" "NOPE")))

  (org-agenda-files
   '("~/benley@gmail.com/org"
     "~/p/xkrd/benley/notes"))

  (org-capture-templates
   '(("n" "Note" entry (file+headline "" "unfiled")
      "* NOTE %?\n  %U" :empty-lines 1)
     ("N" "Note+paste" entry (file+headline "" "unfiled")
      "* NOTE %?\n  %U\n  %c" :empty-lines 1)
     ("t" "Task" entry (file+headline "" "Tasks")
      "* TODO %?\n  %U\n  %a" :empty-lines 1)
     ("T" "Task+paste" entry (file+headline "" "Tasks")
      "* TODO %?\n  %U\n  %c" :empty-lines 1)
     ("e" "Event" entry (file+headline "" "Events")
      "* EVENT %?\n  %U" :empty-lines 1)))

  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  :bind (:map org-mode-map
              ("C-a" . org-beginning-of-line)
              ("C-e" . org-end-of-line)
              ("C-k" . org-kill-line)))

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode))

(use-package org-journal
  :defer t
  :bind (("C-c j" . org-journal-new-entry))

  :custom
  ;; I think you have to set org-journal-dir before loading
  ;; org-journal for it to work correctly (if so, change this back to :init with (setq ...))
  (org-journal-dir "~/benley@gmail.com/org/journal")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %B %e %Y")
  (org-journal-date-prefix "#+DATE: ")
  (org-journal-time-prefix "* ")
  (org-journal-time-format "%A, %B %e %Y %R %Z")
  (org-journal-hide-entries-p nil))

;; (use-package org-make-toc
;;   ;; :init
;;   ;; (defalias 'second #'cadr)
;;   :hook (org-mode . org-make-toc-mode))

(use-package org-sticky-header
  :disabled t
  :hook
  (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full)
  ;; Child and parent headings are seperated by a /.
  (org-sticky-header-outline-path-separator " / "))



(use-package forge
  :after magit
  :init
  ;; TODO: this can probably come out after updating to emacs 26.3
  (when (< emacs-major-version 27)
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
          ghub-use-workaround-for-emacs-bug nil)))

(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode)
  (help-mode . form-feed-mode)
  :diminish form-feed-mode)



(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package saveplace
  :config
  (customize-set-variable 'save-place-mode t))

(use-package smex
  ;; Put frequently-used commands at the front of ido completion list
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

(use-package ido-grid-mode
  :config
  (customize-set-variable 'ido-grid-mode-keys '(tab backtab up down left right C-n C-p C-s C-r))
  (customize-set-variable 'ido-grid-mode t))

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
  :custom
  (terminal-here-terminal-command '("gnome-terminal"))
  (terminal-here-project-root-function #'projectile-project-root)
  :bind
  ("C-c C-S-t C-S-t" . terminal-here-launch)
  ("C-c C-S-t C-S-p" . terminal-here-project-launch))


;; TREEMACS

(use-package treemacs
  :bind
  ("C-c t" . treemacs-select-window)
  :custom
  (treemacs-is-never-other-window t))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)



(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))



(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-keymap-exceptions '("C-x" "M-x" "C-c"))
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  (vterm-buffer-name-string "vterm %s")
  :hook (vterm-mode . hide-mode-line-mode)
  :bind (:map vterm-mode-map
              ;; make shift-pgup/pgdn work like in a typical terminal
              ("<S-prior>" . scroll-down-command)
              ("<S-next>" . scroll-up-command)))



(use-package ws-butler
  :config
  (customize-set-variable 'ws-butler-global-mode t))

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)

  :mode
  "\\.html?\\'"
  "\\.css\\'"
  "\\.scss\\'")


;; (use-package weechat
;;   :config
;;    (setq weechat-modules '(weechat-button weechat-complete weechat-image weechat-notifications))
;;    (setq weechat-notification-mode t)
;;    (setq weechat-return-always-replace-input nil)
;;    (setq weechat-time-format "%H:%M"))

(use-package yaml-mode
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")


;;; emoji stuff?

;; (defun --set-emoji-font (frame)
;;   "Adjust the font settings of FRAME so Emacs can display emoji properly."
;;   (if (eq system-type 'darwin)
;;       ;; For NS/Cocoa
;;       (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
;;     ;; For Linux
;;     (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") frame 'prepend)))

;; (--set-emoji-font nil)

;; (add-hook 'after-make-frame-functions #'--set-emoji-font)



(use-package simple
  :config
  (customize-set-variable 'column-number-mode 1))                  ;; show column position in modeline

(use-package paren
  :config
  (customize-set-variable 'show-paren-mode 1))                     ;; highlight matching parens

;;; I think this happens automatically now - default is "Use --direct only if ls supports it"
;; (when (eq system-type 'darwin)
;;   (setq dired-use-ls-dired nil))        ;; ls doesn't have --dired on darwin

;;; replaced by ws-butler
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

;; (add-hook 'text-mode-hook #'turn-on-auto-fill)


;; FLYSPELL

(use-package flyspell
  :hook
  ;;;; this is really annoying
  ;; (prog-mode . flyspell-prog-mode)
  (text-mode . turn-on-flyspell)
  (pdf-outline . turn-off-flyspell)
  ;;;; unfortunately this makes a variety of things _extremely slow_
  ;; (text-mode flyspell-buffer)
  :config
  (customize-set-variable 'flyspell-issue-message-flag nil))



;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist `(("." . "~/.emacs.d/.backups")))

;; do not want
(setq-default indent-tabs-mode nil)

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t))

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

(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown the Emacs server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

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

(add-hook 'org-journal-mode-hook #'set-buffer-variable-pitch)

;; (add-hook 'org-mode-hook #'variable-pitch-mode)
;; (add-hook 'org-mode-hook #'visual-line-mode)

;; (add-hook 'org-mode-hook #'set-buffer-variable-pitch)

;; (add-hook 'markdown-mode-hook #'set-buffer-variable-pitch)
;; (add-hook 'Info-mode-hook #'set-buffer-variable-pitch)

(use-package ox-latex
  :custom
  ;; (org-latex-listings 'minted)
  (org-latex-listings t)
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  ;; (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  )

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

(use-package ob-emacs-lisp
  :defer t
  :commands (org-babel-execute:emacs-lisp
             org-babel-expand-body:emacs-lisp))

(use-package ob-shell
  :defer t
  :commands (org-babel-execute:sh
             org-babel-expand-body:sh
             org-babel-execute:bash
             org-babel-expand-body:bash))

(use-package ob-jq
  :defer t
  :commands (org-babel-execute:jq
             org-babel-expand-body:jq))

(use-package ob-dot
  :defer t
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package ob-haskell
  :defer t
  :commands (org-babel-execute:haskell
             org-babel-expand-body:haskell))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (shell . t)
;;    (jq . t)
;;    (ruby . t)
;;    (dot . t)
;;    (latex . t)
;;    (haskell . t)))

(defun benley/org-confirm-babel-evaluate (lang body)
  "Don't prompt before evaluating yaml or dot blocks."
  (not (member lang '("yaml" "dot"))))

(setq org-confirm-babel-evaluate #'benley/org-confirm-babel-evaluate)

;; do-nothing execute function for yaml, so I can use yaml src blocks
;; as input to other code blocks
(defun org-babel-execute:yaml (body params) body)


;; ox-gfm: Export to github-flavored markdown
(use-package ox-gfm
  :after org)

(use-package ox-asciidoc
  :after org)

(use-package ox-rst
  :after org)

(use-package ox-ipynb
  :disabled t
  :after org)

(use-package org
  :mode ("\\.org\\'" . org-mode)

  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda))

  :config
  (add-to-list 'org-link-abbrev-alist '("gmap" . "https://maps.google.com/maps?q=%s"))
  ;; make org bullets clickable, etc (part of org-mode)
  (add-to-list 'org-modules 'org-mouse)

  ;; add github:... links to org mode (my local stuff)
  (add-to-list 'org-modules 'org-github-links))

;; https://emacs.stackexchange.com/questions/18404/can-i-display-org-mode-attachments-as-inline-images-in-my-document
(use-package org-attach
  :after org
  :config
  (add-to-list 'org-link-abbrev-alist '("att" . org-attach-expand-link)))


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

(defun benley/confirm-kill-current-buffer ()
  "Prompt before killing the current buffer."
  (interactive)
  (when (y-or-n-p "Really kill current buffer? ")
    (kill-buffer (current-buffer))))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-resize-factor 1.1)

  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-view-fit-page-to-window)
  :bind
  (:map pdf-view-mode-map ("k" . benley/confirm-kill-current-buffer)))

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
;;   :custom
;;   (slime-contribs '(slime-fancy)))

(setq calendar-latitude "42.3601"
      calendar-longitude "-71.0589"
      calendar-location-name "Boston, MA")



(use-package projectile
  :defer 1
  :custom
  (projectile-project-search-path '("~/p/"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t)
  :delight
  '(:eval (concat "[" (projectile-project-name) "]")))

(use-package visual-fill-column
  ;; :hook
  ;; (visual-line-mode . visual-fill-column-mode)
  :config
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package imenu-list
  :config
  (imenu-list-minor-mode t)
  :custom
  (imenu-list-auto-resize t)
  (imenu-list-size 0.15)
  (imenu-list-mode-line-format nil)
  :bind
  ("C-'" . imenu-list-smart-toggle))

(use-package atomic-chrome
  :defer 1
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-url-major-mode-alist
   '(("github\\.com" . gfm-mode)
     ("reddit\\.com" . markdown-mode)))
  (atomic-chrome-extention-type-list '(atomic-chrome))
  (atomic-chrome-buffer-frame-height 40)
  (atomic-chrome-buffer-frame-width 100)
  :config
  (atomic-chrome-start-server))

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;; (use-package 2048-game)

(use-package udev-mode)

(use-package lsp-mode
  :init
  (defun benley--pyls-setup ()
    (setq lsp-pyls-server-command (nix-executable-find (nix-current-sandbox) "pyls"))
    (lsp-deferred))

  :hook ((sh-mode . lsp-deferred)
         (python-mode . benley--pyls-setup)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c o")
  (lsp-prefer-capf t))

(use-package lsp-haskell
  :defer t
  :hook (haskell-mode . lsp-deferred)
  :custom
  (lsp-haskell-process-wrapper-function
        (lambda (cmd) (apply #'nix-shell-command (nix-current-sandbox) cmd))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window))

(use-package lsp-treemacs
  :after lsp-mode)

(use-package esup
  ;; ;; To use MELPA Stable use ":pin mepla-stable",
  ;; :pin melpa
  :commands (esup)
  :custom
  (esup-depth 0))

(use-package hide-mode-line)

(use-package memsql-dev-setup)

(load-theme 'doom-palenight t)

(message "Finished with init.el")
(provide 'init)
;;; init.el ends here
