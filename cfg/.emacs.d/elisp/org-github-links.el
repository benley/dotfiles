;;; org-github-links.el --- Add support for github:user/repo links to org-mode

;; Copyright (c) 2015 Jon Raphaelson

;; Author: Jon Raphaelson <jon@accidental.cc>
;; Version: 1.0
;; Package-Requires: ((s "1.9.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds support to org-mode for handling the types of autolinking that github
;; in provides:
;;
;; github:user/repo
;; github:user/repo@a738b4b8
;; github:user/repo#45

;;; Code:

(require 'org)
(require 's)

(defvar org-github-links-match-regex
  (rx (seq (group (one-or-more (not (any ?/))))
	   (opt (seq "/"
		     (group (one-or-more (not (any "@#"))))
		     (opt (or
			   (seq (group-n 3 "@") (group-n 4 (one-or-more hex)))
			   (seq (group-n 3 "#") (group-n 4 (one-or-more digit)))))))))
  "The regex to use to match github short-links.")

(defun org-github-links-generate-url (path)
  "Visit the Github repo for PATH."
  (let* ((matches (s-match org-github-links-match-regex path))
	 (user    (nth 1 matches))
	 (repo    (nth 2 matches))
	 (type    (nth 3 matches))
	 (extra   (nth 4 matches)))
    (cond
     ((eq user nil) (format "https://github.com"))
     ((eq repo nil) (format "https://github.com/%s" user))
     ((eq type nil) (format "https://github.com/%s/%s" user repo))
     ((equal type "@") (format "https://github.com/%s/%s/commit/%s" user repo extra))
     ((equal type "#") (format "https://github.com/%s/%s/issues/%s" user repo extra)))))

(defun org-github-links-open (path)
  "Open the url recognized in PATH in a browser."
  (browse-url (org-github-links-generate-url path)))

(defun org-github-links-export (path desc backend)
  "Export the link to the url recognized in PATH, displayed as DESC (or path if nil), to BACKEND."
  (let ((url (org-github-links-generate-url path)))
    (cl-case backend
      (html (format "<a href=\"%s\">%s</a>" url (or desc path)))
      (latex (format "\href{%s}{%s}" url (or desc path)))
      (md (format "[%s](%s)" (or desc path) url)))))

(org-link-set-parameters "github"
                         :follow #'org-github-links-open
                         :export #'org-github-links-export)

(provide 'org-github-links)
;;; org-github-links.el ends here
