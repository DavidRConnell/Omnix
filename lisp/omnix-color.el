;;; omnix-color.el --- Color text -*- lexical-binding: t; -*-

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
;; Color text with Org links. Wherever an output file type could not have color
;; (plain text backends) the fallback in `omnix-color-fallback' is used
;; instead.
;;
;; Colors can be defined with Org keyword #+OMNIX_COLOR: color:code, where
;; color is the name to use in links and code is the HTML color code.
;; If a color isn't defined, the color value will be sent directly to the
;; backend, while the code will be used if the color has been defined.
;;
;; So [[color:red][An important message]] will become
;; \textcolor{red}{An important message} for LaTeX.

;;; Code:

(defvar omnix-color-fallback "**%s**"
  "The format string use when a backend does not support color.

The %s format-argument will be replaced by the link description
A %c argument will be replaced by the color *name* (as written in the link, not
the color code it would expand to). Both arguments are optional.

Example: <%c:%s> would expand to <red:An important message> for the link
[[color:red][An important message]].")

(defvar omnix-color-alist '()
  "An alist associating predefined color names with color codes.")

(defvar omnix-color-transcoder-alist
  `((latex . ,#'omnix-color--transcoder-latex)
    (html . ,#'omnix-color--transcoder-html)
    (t . ,#'omnix-color--transcoder-fallback))
  "An alist associating backends with functions to transcode a color link.

The transcoders should take three arguments, the color name, color code, and
the description and return a string.

Color code is a HTML code if the color has been defined in the
`omnix-color-alist' or with OMNIX_COLOR keyword options, otherwise it is the
same as the color name.")

(defun omnix-color--transcoder-latex (name _ description)
  "Transcode the color NAME and DESCRIPTION pair to a LaTeX textcolor command."
  (format "\\textcolor{%s}{%s}" name description))

(defun omnix-color--transcoder-html (_ code description)
  "Transcode the color CODE and DESCRIPTION pair to an HTML style."
  (format "<a style=\"color:%s;\">%s</a>" code description))

(defun omnix-color--transcoder-fallback (name _ description)
  "Transcode the color NAME and DESCRIPTION pair to plaintext representation."
  (replace-regexp-in-string "%\\([cs]\\)"
			    (lambda (substring)
			      (if (string= (match-string 1 substring) "c")
				  name
				description)) omnix-color-fallback))

(defun omnix-color--remove-prefix (prefix string)
  "Remove PREFIX from the start of STRING if it exists.

If STRING does not start with PREFIX, it is returned unmodified."
  (if (string-prefix-p prefix string)
      (cadr (string-split string prefix))
    string))

(defun omnix-color--exporter (path description backend info)
  "Color link exporter.

PATH is the name of the color, DESCRIPTION is the text to color, and BACKEND is
the export BACKEND to transcode to.

INFO is the org-exporter communication channel plist."
  (let* ((transcoder (omnix--alist-get-backend backend
					       omnix-color-transcoder-alist))
	 (color-alist (plist-get info :omnix-colors))
	 (color-code (alist-get (intern path) color-alist path)))
    (funcall transcoder path color-code description)))

(org-link-set-parameters "color" :export #'omnix-color--exporter)

;; Handle options
(add-to-list 'org-export-options-alist
	     '(:omnix-colors "OMNIX_COLOR" nil nil split))

(defun omnix-color--register-colors (info)
  "Extract colors from INFO plist defined using OMNIX_COLOR keywords."
  (let* ((color-defs (plist-get info :omnix-colors))
	 (color-alist omnix-color-alist))
    (dolist (def color-defs)
      (let ((parts (string-split def ":")))
	(if (not (eq (length parts) 2))
	    (message "Malformed color definition \"%s\"; skipping." def)
	  (push `(,(intern (car parts)) . ,(cadr parts)) color-alist))))
    (setq info (plist-put info :omnix-colors color-alist)))
  info)

(defun omnix-color--setup-latex (info)
  "Use the INFO plist to create the LaTeX preamble.

Adds the xcolor package and transcodes the keyword OMNIX_COLOR definitions to
xcolor's definecolor commands."
  (if (plist-get info :omnix-plain)
      info ; Do not use package xcolor when running in plain mode.
    (let ((color-alist (plist-get info :omnix-colors))
	  (current-headers (plist-get info :latex-header))
	  (new-headers "\\usepackage{xcolor}"))
      (dolist (color-pair color-alist)
	(let ((name (car color-pair))
	      (code (omnix-color--remove-prefix "#" (cdr color-pair))))
	  (setq new-headers
		(concat new-headers "\n"
			(format "\\definecolor{%s}{HTML}{%s}" name code)))))

      (setq info (plist-put info :latex-header
			    (if current-headers
				(concat current-headers "\n" new-headers)
			      new-headers))))))

(defun omnix-color--setup (info backend)
  "Register the colors found in the options of the INFO plist.

And maybe set up the LaTeX preamble if using a LaTeX based BACKEND."
  (omnix-color--register-colors info)
  (if (org-export-derived-backend-p backend 'latex)
      (omnix-color--setup-latex info)
    info))

(add-to-list 'org-export-filter-options-functions
	     #'omnix-color--setup)

(provide 'omnix-color)
;;; omnix-color.el ends here
