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

(defun omnix--alist-keys (alist)
  "Return a list of keys in the ALIST."
  (mapcar #'car alist))

(defun omnix--alist-get-backend (backend alist &optional strict-p)
  "Get ALIST items associated with BACKEND.

If BACKEND is not in the ALIST return item associated with t unless STRICT-P is
non-nil, then return nil."
  (let ((backup (if strict-p nil (alist-get t alist))))
    (alist-get backend alist backup nil
	       (lambda (key search-term)
		 (org-export-derived-backend-p search-term key)))))

;; (defun omnix--add-latex-packages (tree format _)
;;   "Add the packages collected in `omnix--add-latex-packages' to the TREE."
;;   (if (eq format 'latex)
;;       (org-element- keyword)))

;; ;; Make sure this called after other omnix parse tree functions have determined
;; ;; which packages are needed.
;; (add-to-list org-export-filter-parse-tree-functions #'omnix--add-latex-packages
;; 	     'append)

(provide 'omnix--utils)
;;; omnix--utils.el ends here
