;;; init.el --- emacs init
;;; Commentary:
;;; Code:

(require 'package)

;;; nix takes care of package installation for me now
;; (setq package-archives
;;       '(("gnu"   . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "http://melpa.org/packages/")
;;         ("org"   . "http://orgmode.org/elpa/")))
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

;; (unless (require 'use-package nil t)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(setq user-mail-address "benley@gmail.com")

;;(use-package base16-theme
;;  :config (load-theme 'base16-materia t))

(use-package material-theme
 :config (load-theme 'material t))

;;(use-package clojure-mode)
;;(use-package clojure-mode-extra-font-locking)

(use-package company
  :diminish company-mode
  ;; Use company-mode in all buffers (more completion)
  :config (add-hook 'after-init-hook #'global-company-mode))

(use-package company-emoji
  :config (add-to-list 'company-backends 'company-emoji)
          (setq company-emoji-insert-unicode nil))

(use-package diminish)

(use-package emojify)

;;(use-package evil
;;  :config (evil-mode 1))

(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode 1))

(use-package flycheck-pos-tip
  :config (flycheck-pos-tip-mode))

(use-package flycheck-color-mode-line
  :config (flycheck-color-mode-line-mode))

(use-package gitignore-mode)

(use-package go-mode)

(use-package haskell-mode)

(use-package ido-completing-read+
  ;; Really nice completion for commands and whatnot
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching 1)

  :config
  ;; show ido autocomplete options pretty much everywhere, like when you hit M-x
  ;; (ido-ubiquitous-mode 1)
  )

(use-package jsonnet-mode)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode))
  :init (setq markdown-command "pandoc")
  :config (add-hook 'gfm-mode-hook (lambda () (visual-line-mode 1))))

(use-package nix-mode)

(use-package nyan-mode
 :config (nyan-mode 1))

(use-package org-bullets
  :config (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package paredit)

;; (use-package powerline
;;   :config (powerline-default-theme))

(use-package protobuf-mode)

(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme))

(use-package spaceline
  ;; :init
  ;; (setq powerline-default-separator 'wave)
  ;; (setq powerline-gui-use-vcs-glyph t)
  ;; :config
  ;; (require 'spaceline-config)
  ;; (spaceline-compile)
  ;; (spaceline-spacemacs-theme)
  )

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  ;; Put frequently-used commands at the front of ido completion list
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package web-mode
  :init
  (defun custom-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))

  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

  :hook
  web-mode-hook 'custom-web-mode-hook)

(use-package yaml-mode)

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Noto Emoji") frame 'prepend)))

(--set-emoji-font nil)

(add-hook 'after-make-frame-functions '--set-emoji-font)

(tool-bar-mode 0)                       ;; disable toolbar
;; (global-linum-mode 1)                   ;; show line numbers
(global-hl-line-mode 1)                 ;; highlight current line
(column-number-mode 1)                  ;; show column position in modeline
(show-paren-mode 1)                     ;; highlight matching parens

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))        ;; ls doesn't have --dired on darwin

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook (lambda () (turn-on-auto-fill)))

;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; Remember what I had open when I quit
;;(desktop-save-mode 1)

;; do not want
(setq-default indent-tabs-mode nil)

;; oh my god STOP BEEPING
(setq visible-bell 1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)

(global-unset-key (kbd "C-z"))  ;; ctrl-z should *not* background a gui app
(global-set-key (kbd "C-z C-z") 'my-suspend-frame)
(defun my-suspend-frame ()
  "Like 'suspend-frame', but refuses to work on graphical windows."
  (interactive)
  (if (display-graphic-p)
      (message "nope, not backgrounding a gui frame")
    (suspend-frame)))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown the Emacs server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; keybindings for org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'init)
;;; init.el ends here
