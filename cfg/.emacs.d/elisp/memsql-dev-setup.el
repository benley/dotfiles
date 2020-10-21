;;; memsql-dev-setup -- memsql engineering defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; from https://memsql.atlassian.net/wiki/spaces/ENG/pages/316014804/Emacs+setup

;;; Code:

;; (setq-default indent-tabs-mode nil)             ; we indent with spaces only, no tabs
;; (setq-default compile-command "memsql-build make debug --skip-binplace memsql-server") ; set default command for M-x compile
;; (setq-default gdb-create-source-file-list nil)  ; gdb initialization takes a long time without this
;; (setq-default word-wrap t)                      ; wrap long lines at word boundaries for better readability

(require 'cc-vars)

(c-add-style "memsql"
             '("linux"
               (c-basic-offset . 4)
               (c-offsets-alist
                (inline-open . 0)
                (innamespace . 0))))

(add-to-list 'c-default-style
             '(c++-mode . "memsql"))

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; use c++-mode instead of c-mode for .h files

;; Default settings for sql-mysql
;; You can run a mysql/memsql client in Emacs with M-x sql-mysql

;; (setq sql-user "root")
;; (setq sql-password "")
;; (setq sql-server "127.0.0.1")
;; (setq sql-mysql-program "/usr/bin/mysql") ;; You can override the path to the client if necessary

(provide 'memsql-dev-setup)
;;; memsql-dev-setup.el ends here
