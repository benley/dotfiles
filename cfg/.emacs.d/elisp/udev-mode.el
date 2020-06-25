;;; udev-mode --- Major mode for editing udev rules files

;; Author: Benjamin Staffin <benley@gmail.com>
;; Keywords: udev linux

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(defvar udev-match-keywords
  '("ACTION"
    "DEVPATH"
    "KERNEL"
    "NAME"
    "SYMLINK"
    "SUBSYSTEM"
    "DRIVER"
    "KERNELS"
    "SUBSYSTEMS"
    "DRIVERS"
    "TAGS"
    "TAG"
    "TEST"
    ;; "PROGRAM"  ;; this is special - uses PROGRAM="" even though it's a matcher, not a setter
    "RESULT"))

(defvar udev-matchparam-keywords
  '("ATTR"
    "SYSCTL"
    "ATTRS"
    "ENV"
    "CONST"
    "TEST"))

(defvar udev-assign-keywords
  '("NAME"
    "SYMLINK"
    "OWNER"
    "GROUP"
    "MODE"
    "TAG"
    "LABEL"
    "GOTO"
    "OPTIONS"))

(defvar udev-assignparam-keywords
  '("SECLABEL"
    "ATTR"
    "SYSCTL"
    "ENV"
    "RUN"  ;; {(program|builtin)}
    "IMPORT"  ;; {(program|builtin|file|db|cmdline|parent)}
    ))

(defvar udev-font-lock-defaults
  `(((,(concat (regexp-opt udev-match-keywords 'words)
               (regexp-opt '("==" "!=")))
      (1 font-lock-constant-face))

     (,(concat (regexp-opt udev-matchparam-keywords 'words)
               "{\\([_A-Za-z0-9]+\\)}"
               (regexp-opt '("==" "!=")))
      (1 font-lock-constant-face)
      (2 font-lock-variable-name-face))

     (,(concat (regexp-opt udev-assign-keywords 'words)
               (regexp-opt '("+=" "-=" ":=" "=")))
      (1 font-lock-builtin-face))

     (,(concat (regexp-opt udev-assignparam-keywords 'words)
               "{\\([_A-Za-z0-9]+\\)}"
               (regexp-opt '("+=" "-=" ":=" "=")))
      (1 font-lock-builtin-face)
      (2 font-lock-variable-name-face))

     ("\\(PROGRAM\\)=" . (1 font-lock-constant-face)))))

;;;###autoload
(define-derived-mode udev-mode fundamental-mode "udev"
  (setq font-lock-defaults udev-font-lock-defaults)
  (setq comment-start "#")
  (setq comment-end "")

  (modify-syntax-entry ?# "< b" udev-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" udev-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.rules\\'" 'udev-mode))

(provide 'udev-mode)

;;; udev-mode.el ends here
