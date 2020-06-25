;;; udev-mode --- Summary
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t; -*-

(require 'generic-x)

(define-generic-mode
    'udev-mode
  '("#")  ;; comments start with #
  '("ATTRS" "ENV" "SUBSYSTEM" "TAG" "MODE" "GROUP" "KERNEL")  ;; some keywords
  '(("\\(==|+=|!=|=\\)" 1 'font-lock-operator)
    ("{\\([A-Za-z0-9_]+\\)}" 1 font-lock-variable-name-face)
    )
  '("\\.rules\\'")  ;; files for which to activate this mode
  nil ;; other functions to call
  "A mode for udev rules files")
