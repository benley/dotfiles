;;; custom --- Custom stuff
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#efecf4" "#be4678" "#2a9292" "#a06e3b" "#576ddb" "#955ae7" "#576ddb" "#585260"])
 '(ansi-term-color-vector
   [unspecified "#efecf4" "#be4678" "#2a9292" "#a06e3b" "#576ddb" "#955ae7" "#576ddb" "#585260"] t)
 '(mac-option-modifier (quote (:ordinary meta :function alt :mouse alt)))
 '(mac-right-option-modifier nil)
 '(org-journal-file-format "%Y-%m-%d.org")
 '(package-selected-packages
   (quote
    (ox-gfm symon treemacs-projectile projectile slime spacemacs-common spacemacs spacemacs-theme org-pdfview vdiff-magit emacs-libvterm terminal-here org-journal docker eterm-color eterm-256color alert yaml-mode xterm-color web-mode use-package treemacs smooth-scrolling smex rainbow-delimiters protobuf-mode powerline paredit org-jira org-bullets org nix-sandbox nix-mode markdown-mode magit json-mode jq-mode ido-grid-mode ido-completing-read+ idle-highlight-mode htmlize haskell-mode go-mode gitignore-mode git-gutter flycheck-status-emoji flycheck-pos-tip flycheck-color-mode-line evil edit-indirect diminish company-terraform bazel-mode)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "\\(\\(?:[Ii]ssue \\|[Ff]ixe[ds] \\|[Rr]esolve[ds]? \\|[Cc]lose[ds]? \\|[Pp]\\(?:ull [Rr]equest\\|[Rr]\\) \\|(\\)#\\([0-9]+\\))?\\)"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(slack-request-timeout 500)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :weight normal :foundry "fsdf" :slant normal :height 111 :width normal))))
 '(fixed-pitch ((t (:family "PragmataPro"))))
 '(variable-pitch ((t (:weight normal :family "Noto Sans Display")))))

;;; custom.el ends here
