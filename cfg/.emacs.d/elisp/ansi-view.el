;;; ansi-view.el --- ANSI color variant of view-mode

;;; Commentary:
;;; By Benjamin Staffin <benley@gmail.com>
;;; Inspired by emacs-pager.el

;;; Code:
(require 'ansi-color)

(defcustom ansi-view-mode-max-line-coloring 10000
  "Max lines to colorize.  Reduce if performance is bad."
  :group 'ansi-view-mode)

(defun ansi-view-file (file)
  "Open FILE in View mode with ANSI color enabled."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (switch-to-buffer buffer)
    (ansi-color-this-buffer)
    (set-buffer-modified-p nil)
    (view-buffer buffer (and (not had-a-buf) 'kill-buffer-if-not-modified))))

(defun ansi-color-this-buffer ()
  "ANSI colorize the current buffer."
  (ansi-color-apply-on-region (goto-char (point-min))
                              (save-excursion
                                (forward-line ansi-view-mode-max-line-coloring)
                                (point))))

(require 'view)
(define-key view-mode-map "j" 'View-scroll-line-forward)
(define-key view-mode-map "k" 'View-scroll-line-backward)
(define-key view-mode-map "f" 'View-scroll-page-forward)
(define-key view-mode-map "b" 'View-scroll-page-backward)
(define-key view-mode-map "G" 'View-scroll-to-buffer-end)

(provide 'ansi-view)
;;; ansi-view.el ends here
