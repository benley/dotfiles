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
(setq doom-font (font-spec :family "PragmataPro" :height 120))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
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

(use-package! evil
  :config
  ;; h, l, left/right and a few other things can cross line endings
  (setq evil-cross-lines t))

(use-package! nginx-mode
  :config
  (setq nginx-indent-level 2))

(use-package! puppet-mode
  :mode "\\.pp\\'")

;; (use-package! lsp-python-ms
;;   :config
;;   (setq lsp-python-ms-executable (executable-find "python-language-server")))

(use-package! jsonnet-language-server)

(use-package! adoc-mode)

(use-package! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))

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
