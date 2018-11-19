;;; exwm --- Summary
;;; Commentary:
;;; Code:

(require 'exwm)
(require 'exwm-config)
(setq exwm-workspace-number 4)

(require 'benley-exwm-fanciness)

(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
(defun benley/use-x11-title-for-buffer-name? ()
  "Decide between x11 title or class for the current buffer name.
Most windows end up with their class name, but some often have
many windows and the window's actual title is more useful."
  (or ;; (string= "XTerm" exwm-class-name)
      (not exwm-instance-name)
      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
      (string= "gimp" exwm-instance-name)
      ;; All Chrome windows:
      (string= "Google-chrome" exwm-class-name)
      ;; Chrome extensions:
      (string-prefix-p "crx_" exwm-instance-name)))

(defun benley/exwm-update-class-hook ()
  "My exwm-update-class-hook."
  (unless (benley/use-x11-title-for-buffer-name?)
    (exwm-workspace-rename-buffer exwm-class-name)))

(add-hook 'exwm-update-class-hook #'benley/exwm-update-class-hook)

(defun benley/exwm-update-title-hook ()
  "My exwm-update-title-hook."
  (when (benley/use-x11-title-for-buffer-name?)
    (exwm-workspace-rename-buffer exwm-title)))

(add-hook 'exwm-update-title-hook #'benley/exwm-update-title-hook)

(setq exwm-manage-configurations
      '(
        ;; float Authy windows (matching exwm-title doesn't work
        ;; because WM_NAME isn't set until after the windows is
        ;; created, so this manage hook never sees it)
        ((equal exwm-instance-name "crx_gaedmjdfmmahhbjefcbgaolhhanlaolb")
         floating t)
        ;; default to char-mode instead of line-mode
        (t char-mode t
           tiling-mode-line nil)
        ))

(defun benley/exwm-input-line-mode ()
  "Set exwm window to line mode and show the mode line."
  (call-interactively #'exwm-input-grab-keyboard)
  (exwm-layout-show-mode-line))

(defun benley/exwm-input-char-mode ()
  "Set exwm window to char mode and hide the mode line."
  (call-interactively #'exwm-input-release-keyboard)
  (exwm-layout-hide-mode-line))

;; Make exwm windows default to char mode instead of line mode
;; (add-hook 'exwm-manage-finish-hook #'benley/exwm-input-char-mode)

(defun benley/exwm-input-toggle-mode ()
  "Toggle between my customized line and char modes."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (cadr (cadr mode-line-process)) "line")
          (benley/exwm-input-char-mode)
        (benley/exwm-input-line-mode)))))

(defun benley/brightness-up ()
  "Increase brightness."
  (interactive)
  (shell-command-to-string "xbacklight -inc 5"))

(defun benley/brightness-down ()
  "Decrease brightness."
  (interactive)
  (shell-command-to-string "xbacklight -dec 5"))

(defun benley/volume-down ()
  "Decrease audio volume."
  (interactive)
  (shell-command-to-string "amixer --quiet set Master 5%- unmute"))

(defun benley/volume-up ()
  "Increase audio volume."
  (interactive)
  (shell-command-to-string "amixer --quiet set Master 5%+ unmute"))

(defun benley/volume-mute-toggle ()
  "Toggle audio mute."
  (interactive)
  (shell-command-to-string "amixer --quiet set Master toggle"))

(defun benley/audio-mic-mute-toggle ()
  "Toggle microphone mute."
  (interactive)
  (shell-command-to-string "amixer --quiet set Capture toggle"))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ;; ,@(mapcar (lambda (i)
        ;;             `(,(kbd (format "s-%d" i)) .
        ;;               (lambda ()
        ;;                 (interactive)
        ;;                 (exwm-workspace-switch-create ,i))))
        ;;           (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ))

(defun benley/lock-the-screen ()
  "Lock the screen."
  (interactive)
  (shell-command-to-string "xset s activate"))

(defun benley/launch-terminal ()
  "Launch a terminal.  If in a project, use project root as cwd."
  (interactive)
  (if (projectile-project-p)
      (terminal-here-project-launch)
    (terminal-here-launch)))

(defun benley/switch-to-last-buffer ()
  "Switch to last open buffer in the current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(exwm-input-set-key (kbd "s-0") #'delete-window)
(exwm-input-set-key (kbd "s-1") #'delete-other-windows)
(exwm-input-set-key (kbd "s-2") #'split-window-below)
(exwm-input-set-key (kbd "s-3") #'split-window-right)
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-H") #'benley/swap-windows-left)
(exwm-input-set-key (kbd "s-J") #'benley/swap-windows-below)
(exwm-input-set-key (kbd "s-K") #'benley/swap-windows-above)
(exwm-input-set-key (kbd "s-L") #'benley/swap-windows-right)
(exwm-input-set-key (kbd "s-x") #'smex)
(exwm-input-set-key (kbd "s-:") #'eval-expression)
;; (exwm-input-set-key (kbd "s-x k") #'benley/exwm-input-toggle-mode)
;; (exwm-input-set-key (kbd "s-x s-k") #'benley/exwm-input-toggle-mode)
;; (exwm-input-set-key (kbd "s-x g") #'magit-status)
(exwm-input-set-key (kbd "s-t") #'benley/launch-terminal)
(exwm-input-set-key (kbd "C-s-l") #'benley/lock-the-screen)
(exwm-input-set-key (kbd "s-b") #'exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "s-p") #'projectile-command-map)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'benley/brightness-up)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'benley/brightness-down)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'benley/volume-down)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'benley/volume-up)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'benley/volume-mute-toggle)
(exwm-input-set-key (kbd "<XF86AudioMicMute>") #'benley/audio-mic-mute-toggle)
(exwm-input-set-key (kbd "s-<tab>") #'benley/switch-to-last-buffer)

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

(exwm-input-set-key (kbd "s-{") #'benley/move-border-left)
(exwm-input-set-key (kbd "s-}") #'benley/move-border-right)
(exwm-input-set-key (kbd "s-a") #'benley/move-border-up)
(exwm-input-set-key (kbd "s-z") #'benley/move-border-down)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist
      '(0 "eDP1" 1 "DP1" 2 "DP2"))

(defun benley/randr-screen-change-hook ()
  "My exwm xrandr screen change hook."
  (start-process-shell-command "xrandr" nil "~/.screenlayout/home.sh"))

(add-hook 'exwm-randr-screen-change-hook #'benley/randr-screen-change-hook)

(exwm-randr-enable)

(require 'exwm-systemtray)
;; (setq exwm-systemtray-height 26)  ;; auto
(exwm-systemtray-enable)

(exwm-enable)
(exwm-config-ido)
(exwm-config-misc)

(setq window-divider-default-right-width 6)
(setq window-divider-default-places t)
(window-divider-mode t)
(display-time)

(setq exwm-floating-border-width 3)
(setq exwm-floating-border-color "yellow")

(use-package symon
  :ensure t
  :config
  (setq symon-monitors
        '(symon-current-time-monitor
          symon-linux-battery-monitor
          symon-linux-memory-monitor
          symon-linux-cpu-monitor
          symon-linux-network-rx-monitor
          symon-linux-network-tx-monitor))
  (setq symon-refresh-rate 5)
  (setq symon-sparkline-height 26)
  (setq symon-sparkline-ascent 'center)
  (symon-mode)
  )

(setq exwm-layout-show-all-buffers t)
(setq exwm-workspace-show-all-buffers t)

;;; exwm.el ends here
