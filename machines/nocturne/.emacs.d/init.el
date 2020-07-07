(setq gc-cons-threshold 20000000)

(when (< emacs-major-version 27)
  ;; Work around the color emoji Xft bug (possibly fixed in 26.3? Probably fixed in 27.)
  (add-to-list 'face-ignored-fonts "Noto Color Emoji"))

(when (< emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(when (< emacs-major-version 27)
  (package-initialize))

(eval-when-compile
  (unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)  ; simple.el
(setq require-final-newline t)   ; files.el
(setq load-prefer-newer t)

(use-package delsel
  :custom
  ;; overwrite selected text on insert
  (delete-selection-mode 1))

(use-package xt-mouse
  :custom
  ;; Enable mouse input in terminals
  (xterm-mouse-mode t))

(use-package saveplace
  :custom
  (save-place-mode t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21242b" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(company-box-icons-alist (quote company-box-icons-all-the-icons) t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" default)))
 '(fci-rule-color "#5B6268")
 '(flycheck-posframe-border-width 2)
 '(flycheck-posframe-warning-prefix "⚠ ")
 '(ido-everywhere t)
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lsp-prefer-capf t t)
 '(lsp-ui-sideline-show-diagnostics nil t)
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (ssh-config-mode yaml-mode treemacs-magit treemacs-projectile treemacs rainbow-delimiters magit flx-ido gitignore-mode gitconfig-mode gitattributes-mode git-gutter solaire-mode doom-modeline doom-themes all-the-icons company-box company-posframe which-key company-lsp lsp-ui lsp-mode smex json-mode nix-mode elm-mode use-package diminish)))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(show-paren-mode t)
 '(solaire-global-mode t)
 '(solaire-mode-auto-swap-bg t)
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package doom-themes
  :ensure t
  :after solaire-mode centaur-tabs
  :config
  (load-theme 'doom-palenight t)
  (require 'doom-themes-ext-treemacs)
  (doom-themes-org-config)
  ;; (solaire-mode-swap-bg)
  :custom
  (doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-theme "doom-colors"))

(use-package doom-modeline
  :ensure t
  :after all-the-icons doom-themes
  :hook (server-create-window-system-frame . doom-modeline-mode)
  :init (doom-modeline-mode t))

(use-package elm-mode
  :ensure t
  :init
  (add-hook 'elm-mode-hook
	    (lambda ()
	      (setq-local prettify-symbols-alist
			  '(("\\" . ?λ)
			    ("->" . ?→)))
	      (turn-on-prettify-symbols-mode))))

(use-package json-mode
  :ensure t)

(use-package nix-mode
  :ensure t
  :custom
  (nix-indent-function #'nix-indent-line)
  :mode
  ;; ("\\.nix\\'" . #'nix-mode)
  ("\\.drv\\'" . #'nix-drv-mode))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-elm
  :disabled t
  :ensure t
  :after flycheck
  :config (flycheck-elm-setup))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-warning-prefix "\u26a0 ")
  (flycheck-posframe-error-prefix "\u274c ")
  (flycheck-posframe-border-width 2)
  :config
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)
  (set-face-attribute 'flycheck-posframe-border-face nil :inherit 'warning))

(use-package git-gutter
  :after magit  ;; just to avoid the "vc-revert got redefined" warning
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode t))

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package centaur-tabs
  :ensure t
  :after all-the-icons
  :demand
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-height 45)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  ("C-<tab>" . centaur-tabs-forward)
  ("C-S-<iso-lefttab>" . centaur-tabs-backward)
  :config
  (centaur-tabs-group-by-projectile-project)

  (defun benley/centaur-tabs-hide-tab-wrapper (orig-fn &rest args)
    "Wrapper for the default centaur-tabs-hide-tab function."
    (let ((name (format "%s" (car args))))
      (or (string-prefix-p "*Ilist*" name)
          (string-prefix-p "*Completions*" name)
          (string-equal "TAGS" name)
          (apply orig-fn args))))

  (advice-add 'centaur-tabs-hide-tab :around #'benley/centaur-tabs-hide-tab-wrapper)

  :hook
  (imenu-list-major-mode . centaur-tabs-local-mode)
  (server-create-window-system-frame . centaur-tabs-headline-match))

(use-package company
  :ensure t
  :diminish company
  :hook (after-init . global-company-mode))

(use-package company-box
  :ensure t
  :after all-the-icons
  :hook (company-mode . company-box-mode)
  :custom (company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package company-posframe
  :disabled t
  :ensure t
  :hook (company-mode . company-posframe-mode))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

(use-package ido
  :custom
  (ido-mode 'both)
  (ido-everywhere t)
  (ido-enable-flex-matching t)
  ;; (ido-use-faces nil)   ; let flx-ido handle faces
  (ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "bazel-.*/"))
  (ido-auto-merge-work-directories-length -1))

(use-package flx-ido
  :ensure t
  :custom
  (flx-ido-mode t))

(defun benley/set-left-fringe-width ()
  (setq left-fringe-width 20))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-completing-read-function #'magit-ido-completing-read)
  (magit-section-visibility-indicator
   '(magit-fringe-bitmap-bold> . magit-fringe-bitmap-boldv))
  :hook (magit-mode . benley/set-left-fringe-width))

(use-package lsp-mode
  :ensure t
  :hook ((elm-mode . lsp)
	 (sh-mode . lsp)
	 (haskell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-prefer-capf t)
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-diagnostics nil)
  :config
  (define-key lsp-ui-mode-map (kbd "C-'") 'lsp-ui-imenu))

(use-package lsp-haskell
  :ensure t
  :after lsp-mode lsp-ui)

(use-package company-lsp
  :disabled t
  :ensure t
  :commands company-lsp)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package which-key
  :ensure t
  :diminish
  which-key-mode
  :config
  (which-key-mode t)
  ;; (which-key-setup-side-window-right-bottom)
  )

(use-package paren
  :custom
  (show-paren-mode t))

(use-package solaire-mode
  :ensure t
  :hook
  ;; I'm not sure if this first set of hooks is necessary, or if
  ;; solaire-global-mode takes care of it
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :custom
  (solaire-mode-auto-swap-bg t)
  (solaire-global-mode t))


(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :ensure t
  :custom
  (projectile-project-search-path '("~/p/"))
  (projectile-mode t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :delight
  '(:eval (concat "[" (projectile-project-name) "]")))

(use-package treemacs
  :ensure t
  :bind
  ("C-c t" . treemacs-select-window)
  :custom
  (treemacs-is-never-other-window t))

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

(use-package yaml-mode
  :ensure t)

(use-package simple
  :custom
  (column-number-mode t)
  (save-interprogram-paste-before-kill t)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  (async-shell-command-buffer 'new-buffer)
  :hook
  (before-save . delete-trailing-whitespace))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package mouse
  :custom
  ;; middle-click paste where the cursor is, not
  ;; wherever the mouse happens to be pointing at
  ;; the time
  (mouse-yank-at-point t))

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-learn-basic-offset 'usually))

(use-package ssh-config-mode
  :ensure t
  :mode
  ("/\\.ssh/config\\'" . ssh-config-mode)
  ("/sshd?_config\\'" . ssh-config-mode)
  ("/known_hosts\\'" . ssh-known-hosts-mode)
  ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :hook
  (ssh-config-mode . turn-on-font-lock))  ; is this even necessary?

(use-package prog-mode
  :preface
  (defun benley/prog-mode-hook ()
    "Setup stuff for `prog-mode' derivatives."
    (if window-system (hl-line-mode t))
    (setq show-trailing-whitespace t)
    (setq display-line-numbers t)
    (when (>= emacs-major-version 27)
	  ;; (setq display-fill-column-indicator t)
	  (setq display-fill-column-indicator-character ?\u2502)))

  :hook
  (prog-mode . benley/prog-mode-hook))

(use-package js
  :custom
  (js-indent-level 2))

(use-package form-feed
  :ensure t
  :hook
  (emacs-lisp-mode . form-feed-mode)
  (help-mode . form-feed-mode)
  :diminish form-feed-mode)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; mouse back/fwd buttons
(global-set-key [mouse-8] #'previous-buffer)
(global-set-key [mouse-9] #'next-buffer)

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
