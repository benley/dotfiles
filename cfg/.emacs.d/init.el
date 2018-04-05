;;; init.el --- emacs init
;;; Commentary:
;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/elisp")

(global-unset-key (kbd "C-z"))     ;; ctrl-z should *not* freeze a gui app
(global-unset-key (kbd "C-x C-z")) ;; why the heck would I ever want to suspend-frame
;; (global-set-key (kbd "C-z C-z") 'my-suspend-frame)
;; (defun my-suspend-frame ()
;;   "Like 'suspend-frame', but refuses to work on graphical windows."
;;   (interactive)
;;   (if (display-graphic-p)
;;       (message "nope, not backgrounding a gui frame")
;;     (suspend-frame)))

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

(require 'ansi-view)

(use-package base16-theme
  :config (load-theme 'base16-materia t))

(use-package bazel-mode
  :config (add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode)))

;; (use-package material-theme
;;  :config (load-theme 'material t))

;;(use-package clojure-mode)
;;(use-package clojure-mode-extra-font-locking)

(use-package company
  :diminish company-mode
  ;; Use company-mode in all buffers (more completion)
  :config (add-hook 'after-init-hook #'global-company-mode))

(use-package company-terraform
  :config
  (company-terraform-init))

(use-package diminish)

;; (use-package flycheck
;;   :config
;;   (global-flycheck-mode 1))

(require 'flycheck)
(global-flycheck-mode 1)
(setq flycheck-ghc-stack-use-nix t)

(use-package flycheck-pos-tip
  :config (flycheck-pos-tip-mode))

(use-package flycheck-color-mode-line
  :after flycheck
  :config
  (flycheck-color-mode-line-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-status-emoji
  :config (flycheck-status-emoji-mode t))

(use-package git-gutter
  :config (global-git-gutter-mode +1))

(use-package gitignore-mode)

(use-package go-mode)

(use-package haskell-mode)

(ido-mode 1)
(ido-everywhere 1)
;; (use-package ido-completing-read+
;;   ;; Really nice completion for commands and whatnot
;;   :init
;;   (setq ido-enable-flex-matching 1)

;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   ;; show ido autocomplete options pretty much everywhere, like when you hit M-x
;;   ;; (ido-ubiquitous-mode 1)
;;   )

;;(use-package jsonnet-mode)
(require 'jsonnet-mode)

(use-package jq-mode
  :mode (("\\.jq$" . jq-mode)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode))
  :init (setq markdown-command "pandoc")
  :config (add-hook 'gfm-mode-hook (lambda () (visual-line-mode 1))))

(require 'mtail-mode)
(add-to-list 'auto-mode-alist (cons "\\.mtail$" #'mtail-mode))

(use-package nix-mode)

(use-package nix-sandbox)
(require 'nix-sandbox)

(setq haskell-process-wrapper-function
      (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

;; Remove after https://github.com/travisbhartwell/nix-emacs/pull/45 is merged:
(defun nix-shell-command (sandbox &rest args)
  "Assemble a command to be executed in SANDBOX from ARGS."
  (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox)
                            (mapconcat 'shell-quote-argument args " "))))

(setq flycheck-command-wrapper-function
      (lambda (cmd) (apply 'nix-shell-command (nix-current-sandbox) cmd)))

(setq flycheck-executable-find
      (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

;; This is _really slow_ for some reason
;; (use-package org-bullets
;;   :config (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package paredit)

(use-package powerline
  :init
  (setq powerline-default-separator 'wave)
  (setq powerline-gui-use-vcs-glyph t)
  :config
  (powerline-default-theme))

(use-package protobuf-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t))))

(use-package smex
  ;; Put frequently-used commands at the front of ido completion list
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 2)
  :config
  (smooth-scrolling-mode 1))

(use-package ido-grid-mode
  :init
  (setq ido-grid-mode-keys (quote (tab backtab up down left right C-n C-p C-s C-r)))
  :config
  (ido-grid-mode +1)
  )

(use-package treemacs
  :bind
  ("C-c t" . treemacs-select-window))

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
;; (desktop-save-mode 1)

;; do not want
(setq-default indent-tabs-mode nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)

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

;; Scrolling
(global-set-key [up] (lambda () (interactive) (previous-line)))
(global-set-key [down] (lambda () (interactive) (next-line)))
(global-set-key [S-up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [S-down] (lambda () (interactive) (scroll-up 1)))

;; It's annoying to see all the "<mouse-N> is undefined" errors when
;; trying to scroll vertically on a trackpad in emacs 25, so tell
;; emacs to ignore it for now.
;; Left/right mwheel support will show up in emacs 26.
;; See https://github.com/emacs-mirror/emacs/commit/88f43dc30cb8d71830e409973cafbaca13a66a45
(global-set-key [mouse-6] 'ignore)
(global-set-key [mouse-7] 'ignore)

;; FONTS
;; -----
(defun set-buffer-variable-pitch ()
  "Set variable-pitch font using `customize-face`.
Set the fonts to format correctly for specific modes.
Default face is fixed so we only need to have the exceptions."
  (interactive)
  (variable-pitch-mode t)
  ;; (setq line-spacing 3)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-link nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch))

;; (add-hook 'org-mode-hook 'set-buffer-variable-pitch)
;; (add-hook 'markdown-mode-hook 'set-buffer-variable-pitch)
;; (add-hook 'Info-mode-hook 'set-buffer-variable-pitch)

;; (require 'ox-latex)
;; (setq org-latex-listings nil)
;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
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

(defun expose-global-binding-in-term (binding)
  "Expose BINDING from the global keymap in term-mode."
  (define-key term-raw-map binding
    (lookup-key (current-global-map) binding)))

(expose-global-binding-in-term (kbd "M-x"))
(define-key term-raw-map (kbd "C-c M-x")
  (lookup-key (current-global-map) (kbd "M-x")))

(load "~/.emacs.d/localonly.el")
(provide 'init)
;;; init.el ends here
