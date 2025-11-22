;;; omnix-glossaries --- elisp port of LaTeX glossary package -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

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
;; A partial port of the LaTeX glossary package for use in Org files.

;;; Code:

(require 'ox)
(require 'omnix--utils)

;; Public vars.
(defvar omnix-glossary-acronym-alist '()
  "Default acronyms to use.

Acronyms should be in the form of (key . (short long)) where the key is a
symbol representing what will be used in the link, short is the acronym and
long is the fully written out form.")

(defvar omnix-glossary-processor-alist '((t . plain))
  "List of processors for each backend.

Should be a list of pairs between backend the processor name with the last
value being the default.

The PLAIN processor works for all backends. With this processor, all glossary
links are transformed to their final form by this OMNIX in the filter stage of
org-export.

LaTeX has the additional backend GLS. When this is used glossary links will be
forwarded to the LaTeX glossary package instead of processed by OMNIX. For
instance `[[gls:lol]]' will be transformed to `\gls{lol}'.

For this behavior set the variable to: `((latex gls) (t plain))'")

(defvar omnix-glossary-link-p nil
  "If non-nil, add links to acronym for backends that support it.

This can be set on a per buffer basis using the OMNIX-LINK-ACRONYMS header
option.")

;; Private vars and helper functions.
(defvar omnix-gloss--acronym-alist '()
  "List of acronyms in short and long form.")

(defvar omnix-gloss--acronym-seen-alist '()
  "List storing acronyms that have been expanded (defined) already.")

(defvar omnix-gloss--link-p nil)

(defun omnix-gloss--acronym-seen-p (key &optional dry-run)
  "Determine if the KEY has been expanded yet.

Unless DRY-RUN is non-nil, mark this KEY as seen."
  (let ((seen-p (alist-get (intern key) omnix-gloss--acronym-seen-alist)))
    (unless (or dry-run seen-p)
      (add-to-list 'omnix-gloss--acronym-seen-alist (list (intern key) t)))
    seen-p))

(defun omnix-gloss--clean (_)
  "Reset globals used for tracking glossary elements."
  (setq omnix-gloss--acronym-alist omnix-glossary-acronym-alist
	omnix-gloss--acronym-seen-alist '()))

(defun omnix-gloss--get-list (key)
  "Get the values for the glossary item associated with KEY."
  (let ((val (alist-get (intern key) omnix-gloss--acronym-alist)))
    (if (not val) (error (format "No acronym with key \"%s\"" key)))
    val))

(defun omnix-gloss--get-short (key)
  "Get the short form for the glossary item associated with KEY."
  (car (omnix-gloss--get-list key)))

(defun omnix-gloss--get-long (key)
  "Get the long form for the glossary item associated with KEY."
  (cadr (omnix-gloss--get-list key)))

(defun omnix-gloss--register-acronym (key short long)
  "Register the SHORT and LONG forms of the acronym associated with KEY."
  (push (list (intern key) short long) omnix-gloss--acronym-alist))

;;; Set options from header keywords.
(add-to-list 'org-export-options-alist
	     '(:omnix-acronyms "OMNIX_ACRONYM" nil nil newline))
(add-to-list 'org-export-options-alist
	     '(:omnix-glossary-processor "OMNIX_GLOSSARY_PROCESSOR" nil nil split))
(add-to-list 'org-export-options-alist
	     '(:omnix-acronym-link
	       nil "omnix-link-acronyms" omnix-glossary-link-p))

(defun omnix-gloss--register-acronyms (tree _ info)
  "Extract acronyms from INFO plist defined using OMNIX_ACRONYM keywords."
  (let ((acronyms (string-split (plist-get info :omnix-acronyms) "\n")))
    (dolist (def acronyms)
      (let ((parts (string-split def ":")))
	(if (< (length parts) 3)
	    (message "Skipping malformed acronym: %s" def)
	  (pcase-let ((`(,key ,short . ,rest) parts))
	    (omnix-gloss--register-acronym key short (string-join rest ":")))))))
  tree)

(defun omnix-gloss--set-processor (info)
  (let ((processors (plist-get info :omnix-glossary-processor)))
    (dolist (proc processors)
      (let ((parts (string-split proc ":")))
	(if (eq (length parts) 1)
	    (add-to-list 'omnix-glossary-processor-alist `(t ,parts))
	  (add-to-list 'omnix-glossary-processor-alist
		       `(,(intern (car parts)) . ,(intern (cadr parts)))))))))

(defun omnix-gloss--set-options (tree _ info)
  (setq-local omnix-glossary-link-p (plist-get info :omnix-acronym-link))
  (omnix-gloss--set-processor info)
  tree)

(add-hook 'org-export-before-processing-hook #'omnix-gloss--clean)
(add-to-list 'org-export-filter-parse-tree-functions
	     #'omnix-gloss--register-acronyms)
(add-to-list 'org-export-filter-parse-tree-functions
	     #'omnix-gloss--set-options)

;;; Expand acronyms.
(defun omnix-glossary--create-link (type)
  "Create the various glossary link TYPEs."
  (let ((func-name (intern (format "omnix-glossary-%s-export" type)))
	(docstring (format "Export a %s link for Org files.

  PATH is the acronym's KEY.

  Link's of this type should not have a DESCRIPTION. Including a description
  will result in an error.

  FORMAT is the backend." type)))
    (defalias func-name
      (lambda (path description format)
	(:documentation docstring)

	(if description
	    (error "Glossary links do not accept descriptions"))

	(let ((processor
	       (omnix--alist-get-backend
		format omnix-glossary-processor-alist
		(alist-get t omnix-glossary-processor-alist 'plain))))
	  (pcase processor
	    ('gls (omnix-gloss--gls-processor type path format))
	    ('plain (omnix-gloss--plain-processor type path format))
	    (_ (error (format "Unknown processor \"%s\"" processor)))))))
    func-name))

(dolist (type '("gls" "acrlong" "acrshort" "acrfull"))
  (org-link-set-parameters type :export (omnix-glossary--create-link type)))

(defun omnix-gloss--gls-processor (type key _)
  "Transform a glossary link to LaTeX's glossary commands.

TYPE should be the name of a LaTeX command.
KEY is the acronym's KEY."
  (format "\\%s{%s}" type key))

(defun omnix-gloss--plain-processor (type key format)
  "Transform a glossary link to the expanded acronym.

TYPE should be the acronym macro name.
KEY is the acronym's KEY.
FORMAT is the export backend."
  (pcase type
    ("gls" (omnix-gloss--expand-auto key format))
    ("acrlong" (omnix-gloss--expand-long key format))
    ("acrfull" (omnix-gloss--expand-full key format))
    ("acrshort" (omnix-gloss--expand-short key format))))

(defun omnix-gloss--expand-short (key _)
  "Always expand the link to the KEY's short form."
  (format "%s" (omnix-gloss--get-short key)))

(defun omnix-gloss--expand-long (key _)
  "Always expand the link to the KEY's long form."
  (format "%s" (omnix-gloss--get-long key)))

(defun omnix-gloss--expand-full (key _)
  "Always expand the link to the KEY's full acronym definition."
  (format "%s (%s)" (omnix-gloss--expand-long key nil)
	  (omnix-gloss--expand-short key nil)))

(defun omnix-gloss--expand-auto (key format)
  "Insert the definition or acronym depending on if KEY has been used yet.

If this is the first time the acronym has been used with the gls link type,
insert the full definition, otherwise insert only the short form.

FORMAT is the export backend for link creation if glossary linking is set."
  (if (omnix-gloss--acronym-seen-p key)
      (omnix-gloss--maybe-link-to-anchor
       (omnix-gloss--link-name key)
       (omnix-gloss--expand-short key nil) format)
    (omnix-gloss--maybe-make-link-anchor
     (omnix-gloss--link-name key)
     (omnix-gloss--expand-full key nil) format)))

(defun omnix-gloss--link-name (key)
  "Name the link anchor for KEY."
  (format "omnix-gls-%s" key))

(defun omnix-gloss--maybe-make-link-anchor (key text format)
  "Add anchor KEY around TEXT for backend FORMAT if glossary linking is on."
  (let ((linker (omnix--alist-get-backend
		 format omnix-create-link-functions-alist)))
    (if (and omnix-glossary-link-p linker)
	(funcall linker key text) text)))

(defun omnix-gloss--maybe-link-to-anchor (key text format)
  "Link TEXT to KEY's anchor for backend FORMAT if glossary linking is on."
  (let ((linker (omnix--alist-get-backend format omnix-link-to-functions-alist)))
    (if (and omnix-glossary-link-p linker)
	(funcall linker key text) text)))

;;; Expand acronym list and glossary macros.
;; I think this is the hook needed to expand (org-export-filter-keyword-functions)

(provide 'omnix-glossaries)
;;; omnix-glossaries.el ends here
