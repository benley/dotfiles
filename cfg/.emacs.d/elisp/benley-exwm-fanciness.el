;;; benley-exwm-fanciness --- exwm helper functions
;;; Commentary:
;;; Code:

(require 'exwm)

(defun benley/swap-windows (&optional w1 w2)
  "If 2 windows are up, swap them.
If W1 is a window, swap it with the current window.
If W2 is also a window, swap both."
  (interactive) ;; unnecessary?
  (unless (or (= 2 (count-windows))
              (windowp w1)
              (windowp w2))
    (error "Ambiguous window selection"))
  (let* ((w1 (or w1 (car (window-list))))
         (w2 (or w2
                 (if (eq w1 (car (window-list)))
                     (nth 1 (window-list))
                   (car (window-list)))))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (with-temp-buffer
      ;; Some buffers like EXWM buffers can only be in one live buffer
      ;; at once.  Switch to a dummy buffer in w2 so we don't display
      ;; any buffer twice.
      (set-window-buffer w2 (current-buffer))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1))
    (set-window-start w1 s2)
    (set-window-start w2 s1))
  (select-window w1))

(defun benley/swap-windows-left ()
  "Swap current window with the one to its left."
  (interactive)
  (benley/swap-windows (window-in-direction 'left)))

(defun benley/swap-windows-right ()
  "Swap current window with the one to its right."
  (interactive)
  (benley/swap-windows (window-in-direction 'right)))

(defun benley/swap-windows-above ()
  "Swap current window with the one above it."
  (interactive)
  (benley/swap-windows (window-in-direction 'above)))

(defun benley/swap-windows-below ()
  "Swap current window with the one below it."
  (interactive)
  (benley/swap-windows (window-in-direction 'below)))

(defun benley/move-border-left-or-right (arg dir-left)
  "Wrapper behind `move-border-left' and `move-border-right'.
ARG is the number of columns to move.  If DIR-LEFT is t then move
left, otherwise move right."
  (unless arg (setq arg 5))
  (let ((at-left-edge (= (car (window-edges)) 0)))
    (if (or (and at-left-edge dir-left)
            (and (not at-left-edge) (not dir-left)))
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun benley/move-border-up-or-down (arg dir-up)
  "Move border ARG lines up or down.
If DIR-UP is t then move up, otherwise move down."
  (unless arg (setq arg 2))
  (let ((at-top-edge (= (nth 1 (window-edges)) 0)))
    (if (or (and at-top-edge dir-up)
            (and (not at-top-edge) (not dir-up)))
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun benley/move-border-up (arg)
  (interactive "P")
  (benley/move-border-up-or-down arg t))

(defun benley/move-border-down (arg)
  (interactive "P")
  (benley/move-border-up-or-down arg nil))

(defun benley/move-border-left (arg)
  "Move window border in a natural manner.
If this is a window with its right edge being the edge of the
screen, enlarge the window horizontally.  If this is a window
with its left edge being the edge of the screen, shrink the
window horizontally.  Otherwise, default to enlarging
horizontally.

Enlarge/Shrink by ARG columns, or 5 if ARG is nil."
  (interactive "P")
  (benley/move-border-left-or-right arg t))

(defun benley/move-border-right (arg)
  "See `move-border-left'.
Enlarge/shrink by ARG columns, or 5 if ARG is nil."
  (interactive "P")
  (benley/move-border-left-or-right arg nil))

(defun benley/exwm-next-workspace ()
  "Switch to the next workspace."
  (interactive)
  (exwm-workspace-switch (+ exwm-workspace-current-index 1)))

(defun benley/exwm-prev-workspace ()
  "Switch to the previous workspace."
  (interactive)
  (exwm-workspace-switch (- exwm-workspace-current-index 1)))

(provide 'benley-exwm-fanciness)
;;; benley-exwm-fanciness.el ends here
