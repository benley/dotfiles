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
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))
;; (setq package-archives nil)
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
  :mode "BUILD\\'" "WORKSPACE\\'" "\\.bzl\\'")

(use-package company
  :diminish company-mode
  ;; Use company-mode in all buffers (more completion)
  :hook (after-init . global-company-mode))

(use-package company-terraform
  :config
  (company-terraform-init))

(use-package diminish)

;; (use-package docker
;;   :config (docker-global-mode))

(use-package flycheck
  :config
  (setq flycheck-ghc-stack-use-nix t)
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

(use-package gitignore-mode)

(use-package go-mode)

(use-package haskell-mode
  :config (setq haskell-tags-on-save t))

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching 1)
(setq ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "bazel-.*/"))
(setq ido-auto-merge-work-directories-length -1)

;;(use-package jsonnet-mode)
(require 'jsonnet-mode)

(use-package jq-mode
  :mode "\\.jq$")

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (setq magit-completing-read-function #'magit-ido-completing-read))

(use-package magit-gh-pulls
  :hook (magit-mode . turn-on-magit-gh-pulls))

(defun enable-visual-line-mode ()
  "Does the obvious thing."
  (visual-line-mode 1))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'" . gfm-mode)
  :config (setq markdown-command "pandoc")
  :hook (gfm-mode . enable-visual-line-mode))

(require 'mtail-mode)
(add-to-list 'auto-mode-alist (cons "\\.mtail$" #'mtail-mode))
(add-to-list 'auto-mode-alist (cons "\\.em$" #'mtail-mode))

(use-package nix-mode)

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

;; This is _really slow_ for some reason
;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;;   :config (setq (org-bullets-face-name (quote fixed-pitch))))

(use-package org-journal
  :config
  (setq org-journal-dir "~/Dropbox/org/journal")
  (setq org-journal-file-format "%Y-%m-%d.org"))

(use-package paredit)

(use-package powerline
  :init
  (setq powerline-default-separator 'wave)
  (setq powerline-gui-use-vcs-glyph t)
  :config
  (powerline-default-theme))

(use-package protobuf-mode)

(use-package rainbow-delimiters
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

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 2)
  :config
  (smooth-scrolling-mode 1))

(use-package ido-grid-mode
  :init
  (setq ido-grid-mode-keys '(tab backtab up down left right C-n C-p C-s C-r))
  :config
  (ido-grid-mode +1))

(use-package treemacs
  :bind
  ("C-c t" . treemacs-select-window))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

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

(use-package weechat)

(use-package yaml-mode)

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Noto Emoji") frame 'prepend)))

(--set-emoji-font nil)

(add-hook 'after-make-frame-functions #'--set-emoji-font)

(tool-bar-mode 0)
(menu-bar-mode 0)
;; (global-linum-mode 1)                   ;; show line numbers
(global-hl-line-mode 1)                 ;; highlight current line
(column-number-mode 1)                  ;; show column position in modeline
(show-paren-mode 1)                     ;; highlight matching parens

(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))        ;; ls doesn't have --dired on darwin

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist `(("." . "~/.emacs-backups")))

;; do not want
(setq-default indent-tabs-mode nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)

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
(global-set-key "\C-cl" #'org-store-link)
(global-set-key "\C-ca" #'org-agenda)
(global-set-key "\C-cc" #'org-capture)
(global-set-key "\C-cb" #'org-iswitchb)

;; Scrolling
(global-set-key [up] (lambda () (interactive) (previous-line)))
(global-set-key [down] (lambda () (interactive) (next-line)))
(global-set-key [S-up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [S-down] (lambda () (interactive) (scroll-up 1)))

;; It's annoying to see all the "<mouse-N> is undefined" errors when trying to
;; scroll vertically on a trackpad in emacs 25, so tell emacs to ignore it for
;; now.  Left/right mwheel support will show up in emacs 26.  See
;; https://github.com/emacs-mirror/emacs/commit/88f43dc30cb8d71830e409973cafbaca13a66a45
(global-set-key [mouse-6] #'ignore)
(global-set-key [mouse-7] #'ignore)

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
  "Expose BINDING from the global keymap in term-mode."
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
(define-key shell-mode-map (kbd "<up>")   #'comint-previous-input)
(define-key shell-mode-map (kbd "<down>") #'comint-next-input)

;;; Enable some languages that I want to use with org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (jq . t)))

(setq org-M-RET-may-split-line nil)
(setq org-agenda-start-on-weekday 0)
(setq org-catch-invisible-edits 'error)
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-directory "~/Dropbox/org")
(setq org-ellipsis "⤵")
(setq org-footnote-define-inline t)
(setq org-goto-auto-isearch nil)
(setq org-log-done 'time)

(setq org-link-abbrev-alist
      '(("jira" . "https://postmates.atlassian.net/browse/")))

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

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 3)))

(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq sh-learn-basic-offset 'usually)

(defun my-prog-mode-hook ()
  "Setup stuff for 'prog-mode' derivatives."
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t)
  (highlight-indentation-mode t)
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'my-prog-mode-hook)

(defun my-term-mode-hook ()
  "My term-mode hook."
  ;;Attempt to unfuck ansi-term's colors after changing themes
  (setq ansi-term-color-vector
        [term term-color-black
              term-color-red
              term-color-green
              term-color-yellow
              term-color-blue
              term-color-magenta
              term-color-cyan
              term-color-white])
  ;; make URLs clickable
  (goto-address-mode))

;; maybe hopefully work around the thing where ansi-term breaks any time I change themes?
(add-hook 'term-mode-hook #'my-term-mode-hook)

(defun my-term-exec-hook ()
  "Try to make terminals work better with unicode I guess."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

(add-hook 'term-exec-hook #'my-term-exec-hook)

;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))


;; Set the X11 window title like "benley@mintaka: init.el (~/.emacs.d/init.el)"
(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

(load "~/.emacs.d/localonly.el")
(provide 'init)
;;; init.el ends here
