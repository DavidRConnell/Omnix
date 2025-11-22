;;; omnix--utils --- Helper functions for Omnix -*- lexical-binding: t; -*-

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
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; A place for functions that are useful to multiple Omnix modules.

;;; Code:

(require 'ox)

;; Public vars.
(defun omnix-create-link-latex (key target)
  "Create link anchor KEY around TARGET."
  (format "\\hypertarget{%s}{%s}" key target))

(defun omnix-create-link-html (key target)
  "Create link anchor KEY around TARGET."
  (format "<a id=\"%s\">%s</a>" key target))

(defvar omnix-create-link-functions-alist `((latex . ,#'omnix-create-link-latex)
					    (html . ,#'omnix-create-link-html)
					    (md . ,#'omnix-create-link-html))
  "An alist associating backends with a function for creating link anchors.

The function should accept KEY for the anchor name and TARGET for the text to
anchor.

This is only used with plain processors.

See also `omnix-link-to-filter-alist'.")

(defun omnix-link-to-latex (key description)
  "Link to a predefined KEY with DESCRIPTION."
  (format "\\hyperlink{%s}{%s}" key description))

(defun omnix-link-to-html (key description)
  "Link to a predefined KEY with DESCRIPTION."
  (format "<a href=\"#%s\">%s</a>" key description))

(defvar omnix-link-to-functions-alist `((latex . ,#'omnix-link-to-latex)
					(html . ,#'omnix-link-to-html)
					(md . ,#'omnix-link-to-html))
  "An alist associating backends with a function for linking to a link anchor.

The function should take an anchor KEY and a DESCRIPTION to write in the link.

This is only used with plain processors.

See also `omnix-create-link-format-alist'.")

;; Private helpers
(defun omnix--alist-get-backend (backend alist &optional default)
  "Get ALIST items associated with BACKEND.

If BACKEND is not in the ALIST return DEFAULT."
  (alist-get backend alist default nil 'org-export-derived-backend-p))

(provide 'omnix--utils)
;;; omnix--utils.el ends here
