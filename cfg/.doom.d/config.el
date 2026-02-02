;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin Staffin"
      user-mail-address "benley@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "PragmataPro" :size 12.0))
(setq doom-serif-font (font-spec :family "Go Mono" :size 12.0))
(setq doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 12.0))
;; NOTE :size <int> is pixels, :size <float> is points

;; Let darkman handle theme loading instead of doom
;; (setq doom-theme 'doom-palenight)

(setq org-directory "~/Documents/org/")

(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! auth-source
  :config
  (setq auth-sources '(default "secrets:Login")))

(use-package! darkman
  :config
  (setq darkman-themes '(:light leuven
                         :dark doom-palenight))

  ;; When running in daemon mode, wait for the session to be fully initialized
  ;; before darkman does anything, just in case load-theme tries to prompt
  ;; interactively or something.
  (when (daemonp)
    (add-hook 'server-after-make-frame-hook #'darkman-mode)
    (advice-add 'darkman-mode
                :after
                (lambda ()
                  (remove-hook 'server-after-make-frame-hook
                               #'darkman-mode))))

  ;; Replace darkman's theme selection with consult-theme, which is what doom
  ;; uses behind the scenes
  (advice-add 'darkman--load-theme :override #'consult-theme)

  (darkman-mode))

(use-package! frame
  :config
  (setq window-divider-default-right-width 5))

(use-package! evil
  :config
  ;; h, l, left/right and a few other things can cross line endings
  (setq evil-cross-lines t))

(use-package! nginx-mode
  :config
  (setq nginx-indent-level 2))

(use-package! puppet-mode
  :mode "\\.pp\\'")

(use-package! jsonnet-language-server)

(use-package! adoc-mode)

(use-package! undo-fu
  :config
  (setq undo-fu-ignore-keyboard-quit t))

(use-package! udev-mode)

(use-package! atomic-chrome
  :defer 1

  :hook
  (atomic-chrome-edit-mode . +my/atomic-chrome-mode-setup)

  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-url-major-mode-alist
   '(("github\\.com" . gfm-mode)
     ("reddit\\.com" . markdown-mode)
     ("snippets\\.internal\\.memcompute\\.com" . markdown-mode)))
  (atomic-chrome-extention-type-list '(atomic-chrome))
  (atomic-chrome-buffer-frame-height 40)
  (atomic-chrome-buffer-frame-width 100)

  :config
  (defun +my/atomic-chrome-mode-setup ()
    (setq header-line-format
          (substitute-command-keys
           "Editing Chrome test area. Finish with `\\[atomic-chrome-close-current-buffer]'.")))
  (atomic-chrome-start-server))

(use-package! form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode))

(use-package! vterm
  :bind (:map vterm-mode-map
         ;; make shift-pgup/pgdn work like in a typical terminal
         ("<S-prior>" . scroll-down-command)
         ("<S-next>" . scroll-up-command)))

(use-package! info
  :bind
  (:map Info-mode-map
        ("<mouse-8>" . Info-history-back)
        ("<mouse-9>" . Info-history-forward)))

(use-package! comint
  :config
  (setq comint-scroll-to-bottom-on-input 'this))  ;; scroll only _this_ window

;; accept completion from copilot and fallback to company
(use-package! copilot
  :after company
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (add-to-list 'copilot-indentation-alist '(nix-mode 2)))

(use-package! lsp-mode
  :config
  (setq lsp-auto-execute-action nil)
  (setq lsp-haskell-plugin-stan-global-on nil))

(use-package! web-mode
  :mode "\\.ftl\\'")

(after! org
  (setq org-roam-directory "~/Documents/org/roam/")
  (setq org-roam-index-file "~/Documents/org/roam/index.org"))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! magit-status
  :config
  (setq magit-status-margin '(t age-abbreviated magit-log-margin-width t 18)))

(after! tramp-sh
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; font filename workaround https://github.com/doomemacs/doomemacs/issues/7431#issuecomment-1722663411
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))

(use-package! flycheck-actionlint
  :config
  (flycheck-actionlint-setup))

;; TODO: doom has a smooth-scroll module that wraps ultra-scroll now
(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; When tabs are enabled, remap ctrl-pgup/pgdn to switch tabs
;; and then gt/gT can be used to switch workspaces like before
(map! (:when (modulep! :ui tabs)
        :nvie "C-<next>" #'+tabs:next-or-goto
        :nvie "C-<prior>" #'+tabs:previous-or-goto
        :n "gt" #'+workspace:switch-next
        :n "gT" #'+workspace:switch-previous))

(use-package! lsp-terraform
  :config
  (setq lsp-terraform-ls-server "tofu-ls"))
