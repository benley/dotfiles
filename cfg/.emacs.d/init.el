(require 'cl)
(require 'package)

(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    company
    evil
    haskell-mode
    ido-ubiquitous
    magit
    nix-mode
    noctilux-theme
    paredit
    ;;projectile
    puppet-mode
    rainbow-delimiters
    ;;smex
    ;;tagedit
    web-mode
    anti-zenburn-theme
    yaml-mode
    zenburn-theme
    solarized-theme
    flycheck
    flycheck-color-mode-line
    flycheck-pos-tip
    powerline
    ;;(if (eq system-type 'darwin) exec-path-from-shell)
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;(require 'evil)
;(evil-mode 1)

(require 'powerline)
(powerline-default-theme)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(require 'flycheck-color-mode-line)
(flycheck-color-mode-line-mode)

(global-flycheck-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'base16-materia t)

(tool-bar-mode 0)                       ;; disable toolbar
(global-linum-mode 1)                   ;; show line numbers
(global-hl-line-mode 1)                 ;; highlight current line
(column-number-mode 1)                  ;; show column position in modeline
(show-paren-mode 1)                     ;; highlight matching parens
(rainbow-delimiters-mode 1)             ;; ((((rainbows))))

(if (eq system-type 'darwin)
    (setq dired-use-ls-dired nil))      ;; osx ls doesn't have --dired

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook (lambda () (turn-on-auto-fill)))

;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

;; Really nice completion for commands and whatnot
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Use company-mode in all buffers (more completion)
(add-hook 'after-init-hook 'global-company-mode)

;; Remember what I had open when I quit
(desktop-save-mode 1)

;; do not want
(setq-default indent-tabs-mode nil)

;; shut the FUCK UP
(setq visible-bell 1)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'custom-web-mode-hook)

(global-set-key (kbd "C-x g") 'magit-status)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" default)))
 '(mac-option-modifier (quote (:ordinary meta :function alt :mouse alt)))
 '(mac-right-option-modifier nil)
 '(package-selected-packages
   (quote
    (base16-theme powerline flycheck-pos-tip flycheck-color-mode-line flycheck solarized-theme zenburn-theme yaml-mode anti-zenburn-theme web-mode rainbow-delimiters puppet-mode paredit noctilux-theme nix-mode magit ido-ubiquitous haskell-mode evil company cider clojure-mode-extra-font-locking clojure-mode)))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Operator Mono" :foundry "H&Co" :slant normal :weight normal :height 111 :width normal))))
 '(mouse ((t (:background "white")))))

(provide 'init)
;;; init.el ends here
