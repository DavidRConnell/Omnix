;;; omnix-capf.el --- Create CAPFs for Omnix link types -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Created: January 18, 2026

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; Provides a single CAPF to handle all of Omnix's link types. Each link module
;; registers their link name with a function that gets candidates.
;;
;; New CAPFs can be registered by adding an alist associating the name of a
;; link to a function that generates completion candidates to the
;; `omnix-capf-alist'. To add paper-aware completion, see
;; `omnix-search-get-candidates'.

;;; Code:

(require 'org)

(require 'omnix--search)

(defun omnix-capf-setup ()
  "Add the Omnix CAPF to completion at point functions."
  (add-hook 'completion-at-point-functions #'omnix-capf nil t))

(defvar omnix-capf-alist '()
  "Alist associating a link name to a candidate generating function.")

(defun omnix-capf ()
  "Completion at point function for Org links."
  (when (omnix-capf--looking-at-link-p)
    (if (not (match-string 2))
	(let ((start (match-beginning 1)))
	  (list start (point)
		(mapcar #'car org-link-parameters)
		:exclusive 'no))
      (let ((start (match-beginning 2))
	    (candidates-func (alist-get (match-string 1)
					omnix-capf-alist
					nil nil #'string=)))
	(if candidates-func
	    (funcall candidates-func start (point)))))))

(defun omnix-capf--looking-at-link-p ()
  "Test if point is currently in an Org link.

If non-nil, `match-string' will be set."
  (and (eq major-mode 'org-mode)
       (save-excursion
	 (search-backward "[[" (line-beginning-position) t)
	 (looking-at (rx "[[" (group (* (not (or ":" whitespace "]"))))
			 (? ":" (group (* (not "]")))))))))

(provide 'omnix-capf)
;;; omnix-capf.el ends here
