;;; custom --- Custom stuff
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-materia)))
 '(mac-option-modifier (quote (:ordinary meta :function alt :mouse alt)))
 '(mac-right-option-modifier nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/notes.org" "~/.org-jira/INFRASTRUC.org" "~/Dropbox/org/work_calendar.org" "~/Dropbox/org/work.org" "~/Dropbox/org/stuff.org")))
 '(org-bullets-face-name (quote fixed-pitch))
 '(package-selected-packages
   (quote
    (magit-gh-pulls alert slack yaml-mode xterm-color weechat web-mode use-package treemacs smooth-scrolling smex rainbow-delimiters protobuf-mode powerline paredit org-jira org-bullets org nix-sandbox nix-mode markdown-mode magit json-mode jq-mode ido-grid-mode ido-completing-read+ idle-highlight-mode htmlize haskell-mode go-mode gitignore-mode git-gutter flycheck-status-emoji flycheck-pos-tip flycheck-color-mode-line exwm evil elscreen edit-indirect diminish company-terraform bazel-mode base16-theme)))
 '(slack-request-timeout 500)
 '(weechat-modules
   (quote
    (weechat-button weechat-complete weechat-image weechat-notifications)))
 '(weechat-notification-mode t)
 '(weechat-return-always-replace-input nil)
 '(weechat-time-format "%H:%M")
 '(woman-fill-frame t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :weight normal :foundry "fsdf" :slant normal :height 111 :width normal))))
 '(fixed-pitch ((t (:family "PragmataPro"))))
 '(variable-pitch ((t (:weight normal :family "Noto Sans Display")))))

;;; custom.el ends here
