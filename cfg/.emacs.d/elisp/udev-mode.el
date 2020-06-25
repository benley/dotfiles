;;; udev-mode --- Major mode for editing udev rules files

;; Author: Benjamin Staffin <benley@gmail.com>
;; Keywords: udev linux

;;; Commentary:

;;; Code:

;; -*- lexical-binding: t; -*-

(defconst udev-match-keywords
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

(defconst udev-matchparam-keywords
  '("ATTR"
    "SYSCTL"
    "ATTRS"
    "ENV"
    "CONST"
    "TEST"))

(defconst udev-assign-keywords
  '("NAME"
    "SYMLINK"
    "OWNER"
    "GROUP"
    "MODE"
    "TAG"
    "LABEL"
    "GOTO"
    "OPTIONS"))

(defconst udev-assignparam-keywords
  '("SECLABEL"
    "ATTR"
    "SYSCTL"
    "ENV"
    "RUN"  ;; {(program|builtin)}
    "IMPORT"  ;; {(program|builtin|file|db|cmdline|parent)}
    ))

(defconst udev-font-lock-keywords
  `((,(concat (regexp-opt udev-match-keywords 'words)
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

    ("\\(PROGRAM\\)=" . (1 font-lock-constant-face))))

;;;###autoload
(define-derived-mode udev-mode fundamental-mode "udev"
  (setq font-lock-defaults '(udev-font-lock-keywords
                             nil ;; keywords-only
                             nil ;; case-fold
                             nil ;; syntax-alist
                             nil ;; other-vars
                             ))
  (setq comment-start "#")
  (setq comment-end "")

  (modify-syntax-entry ?# "<" udev-mode-syntax-table)
  (modify-syntax-entry ?\n ">" udev-mode-syntax-table))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.rules\\'" 'udev-mode))

(provide 'udev-mode)

;;; udev-mode.el ends here
