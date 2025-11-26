;;; omnix-sansext.el --- Choice figure type based on backend -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; A link type to dynamically choose a figure based on the backend.
;; You can supply the file name with or without an extension and it will
;; add/exchange the extension to the preferred extension for the backend. This
;; should be used for figures which have been generated as multiple types so
;; for example, when exporting to HTML, the SVG version of a figure will be
;; chosen but when exporting to PDF, the PDF version will be used.
;;
;; This is a simple link built around the default image insertion logic, if a
;; figure with the preferred extension does not exist, it will be as if you
;; passed a file that does not exist to the regular file link and the export
;; will fail.

;;; Code:

(require 'omnix--utils)

(defvar omnix-sansext-extension-alist '((latex . "pdf")
					(html . "svg")
					(t . "png"))
  "Preferred figure extension to use for each backend.")

(defun omnix-sansext--replace-extensions (backend)
  "Replace sansext figures to figures with BACKEND preferred extension.

Search and replace all sansext links with regular file links that have the
file's extension replaced (or added) with the preferred extension for the
current export BACKEND."
  (let ((backend-ext (omnix--alist-get-backend backend
					       omnix-sansext-extension-alist))
	(sansext-re "\\[\\[sansext:\\([^]\\]*\\)]\\(?:\\[\\(.*?\\)]\\)?]"))
    (save-excursion
      (while (re-search-forward sansext-re nil t)
	(let* ((filename (match-string 1))
	       (description (match-string 2))
	       (new-name (file-name-with-extension filename backend-ext))
	       (new-path (if description
			     (format "[[file:%s][%s]]" new-name description)
			   (format "[[file:%s]]" new-name))))
	  (replace-match new-path))))))

(add-to-list 'org-export-before-processing-functions #'omnix-sansext--replace-extensions)

(provide 'omnix-sansext)
;;; omnix-sansext.el ends here
