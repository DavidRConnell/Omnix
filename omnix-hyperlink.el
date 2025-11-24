;;; omnix-hyperlink --- Filters for adding links -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell
;;
;; Author: David R. Connell <david32@dcon.addy.io>
;; Version: 0.1.0

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
;; Define various functions for creating links in different backends.

;;; Code:

;; Public vars.
(defun omnix-hl--anchor-latex (key description)
  "Create link anchor KEY around DESCRIPTION."
  (format "\\hypertarget{%s}{%s}" key description))

(defun omnix-hl--anchor-html (key description)
  "Create link anchor KEY around DESCRIPTION."
  (format "<a id=\"%s\">%s</a>" key description))

;; Cannot get odt exporter to work so leaving out of alist until I can test it.
(defun omnix-hl--anchor-odt (key description)
  "Create link anchor KEY around DESCRIPTION."
  (format
   "<text:bookmark-start text:name=\"%s\"/>%s<text:bookmark-end text:name=\"%s\"/>"
   key description key))

(defvar omnix-hl-anchor-functions-alist `((latex . ,#'omnix-hl--anchor-latex)
					  (html . ,#'omnix-hl--anchor-html)
					  (md . ,#'omnix-hl--anchor-html))
  "An alist associating backends with a function for creating link anchors.

The function should accept KEY for the anchor name and DESCRIPTION for the text
to anchor.

See also `omnix-hl-link-functions-alist'.")

(defun omnix-hl--anchor (backend key description)
  "Create an anchor around DESCRIPTION with identifier KEY.

Anchors are created based on the export BACKEND."
  (let ((func (omnix--alist-get-backend
	       backend omnix-hl-anchor-functions-alist 'strict)))
    (funcall func key description)))

(defun omnix-hl--link-latex (key description)
  "Link to a predefined KEY with DESCRIPTION."
  (format "\\hyperlink{%s}{%s}" key description))

(defun omnix-hl--link-html (key description)
  "Link to a predefined KEY with DESCRIPTION."
  (format "<a href=\"#%s\">%s</a>" key description))

(defun omnix-hl--link-md (key description)
  "Link to a predefined KEY with DESCRIPTION."
  (format "[%s](#%s)" description key))

(defun omnix-hl--link-odt (key description)
  "Link to a predefined KEY with DESCRIPTION."
  (format "<text:a xlink:type=\"simple\" xlink:href=\"#%s\">%s</text:a>"
	  key description))

(defvar omnix-hl-link-functions-alist `((latex . ,#'omnix-hl--link-latex)
					(html . ,#'omnix-hl--link-html)
					(md . ,#'omnix-hl--link-md))
  "An alist associating backends with a function for linking to a link anchor.

The function should take an anchor KEY and a DESCRIPTION to write in the link.

See also `omnix-create-link-format-alist'.")

(defun omnix-hl--link (backend key description)
  "Create an anchor around DESCRIPTION with identifier KEY.

Anchors are created based on the export BACKEND."
  (let ((func (omnix--alist-get-backend
	       backend omnix-hl-link-functions-alist 'strict)))
    (funcall func key description)))

(provide 'omnix-hyperlink)
;;; omnix-hyperlink.el ends here
