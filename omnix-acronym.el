;;; omnix-acronym --- Org filter for managing acronyms -*- lexical-binding: t; -*-

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
;; Manages acronyms through Org links. Inspired by the LaTeX glossaries
;; package and `org-cite'.
;;
;; Handles 4 types of acronym links in the form [[acr:tbd]]:
;;   acr :
;;     Auto expansion. If this is the first time the link has been used, fully
;;     expand it, "To Be Determined (TBD)", if it has been used then place the
;;     short form "TBD".
;;   acr/short :
;;     Always writes the short form.
;;   acr/long :
;;     Always writes the long form, "To Be Determined".
;;   acr/full :
;;     Always writes the full form.
;;
;;   Acronyms are defined with the #+OMNIX_ACRONYM: keyword, with one
;;   definition per keyword in the form "key:acronym:long" Such as:
;;
;;   #+OMNIX_ACRONYM: tbd:TBD:To Be Determined
;;
;;   Acronym handling can be modified by selecting a backend specific
;;   processor (processors selection is determined by the
;;   `omnix-acronym-processor-alist' or on a per buffer scope with the
;;   #+OMNIX_ACRONYM_PROCESSOR: backend:processor keyword specification). If a
;;   processor does not know how to handle the backend, it will fallback to
;;   plain.
;;
;;   Processors:
;;     plain :
;;       The default processor used by all backends. The expansion is handled
;;       in the filter stage and results in plain text acronyms.
;;     link :
;;        Same as plain except adds linking back to the original definition
;;        made with the ACR type link. If ACR is used more than once in the
;;        document, an anchor will be wrapped around the original ACR link and
;;        all future ACR link calls are hyperlinked to that anchor.
;;
;;        Depends on backend specific hyperlinking methods so is only available
;;        to those with hl-anchor and hl-link functions defined. This will add
;;        hyperref to LaTeX's packages.
;;      gls :
;;        A LaTeX specific processor that will pass all acronyms to the
;;        glossaries package (consequently adding glossaries to the tex file).
;;
;;    Processors are defined as a plist with ACR, ACR-SHORT, ACR-LONG, and
;;    ACR-FULL functions.

;;; Code:

(require 'ox)
(require 'omnix--utils)

;; Public vars.
(defvar omnix-acronym-alist '()
  "Default acronyms to use.

Acronyms should be in the form of (key . (short long)) where the key is a
symbol representing what will be used in the link, short is the acronym and
long is the fully written out form.")

(defvar omnix-acronym-processor-alist '((t . plain))
  "List of processors for each backend.

Should be a list of pairs between backend the processor name with the last
value being the default.

The PLAIN processor works for all backends. With this processor, all acronym
links are transformed to their final form by this OMNIX in the filter stage of
org-export.

LaTeX has the additional backend GLS. When this is used acronym links will be
forwarded to the LaTeX glossaries package instead of processed by OMNIX. For
instance `[[acr:lol]]' will be transformed to `\gls{lol}'.

For this behavior set the variable to: `((latex gls) (t plain))'")

(defvar omnix-acronym-link-p nil
  "If non-nil, add links to acronym for backends that support it.

This can be set on a per buffer basis using the OMNIX-LINK-ACRONYMS header
option.")

;; Private vars and helper functions.
(defvar omnix-acronym--acronym-alist '()
  "List of acronyms in short and long form.")

(defvar omnix-acronym--acronym-seen-alist '()
  "List storing acronyms that have been expanded (defined) already.")

(defun omnix-acronym--acronym-seen-p (key &optional dry-run)
  "Determine if the KEY has been expanded yet.

Unless DRY-RUN is non-nil, mark this KEY as seen."
  (let ((seen-p (alist-get (intern key) omnix-acronym--acronym-seen-alist)))
    (unless (or dry-run seen-p)
      (add-to-list 'omnix-acronym--acronym-seen-alist (list (intern key) t)))
    seen-p))

(defun omnix-acronym--clean (_)
  "Reset globals used for tracking acronym elements."
  (setq omnix-acronym--acronym-alist omnix-acronym-alist
	omnix-acronym--acronym-seen-alist '()))

(defun omnix-acronym--get-list (key)
  "Get the values for the acronym item associated with KEY."
  (let ((val (alist-get (intern key) omnix-acronym--acronym-alist)))
    (if (not val) (error (format "No acronym with key \"%s\"" key)))
    val))

(defun omnix-acronym--get-short (key)
  "Get the short form for the acronym item associated with KEY."
  (car (omnix-acronym--get-list key)))

(defun omnix-acronym--get-long (key)
  "Get the long form for the acronym item associated with KEY."
  (cadr (omnix-acronym--get-list key)))

(defun omnix-acronym--register-acronym (key short long)
  "Register the SHORT and LONG forms of the acronym associated with KEY."
  (push (list (intern key) short long) omnix-acronym--acronym-alist))

;;; Set options from header keywords.
(add-to-list 'org-export-options-alist
	     '(:omnix-acronyms "OMNIX_ACRONYM" nil nil newline))
(add-to-list 'org-export-options-alist
	     '(:omnix-acronym-processor "OMNIX_ACRONYM_PROCESSOR" nil nil split))
(add-to-list 'org-export-options-alist
	     '(:omnix-acronym-link
	       nil "omnix-link-acronyms" omnix-acronym-link-p))

(defun omnix-acronym--register-acronyms (tree _ info)
  "Extract acronyms from INFO plist defined using OMNIX_ACRONYM keywords."
  (let* ((acronyms-raw (plist-get info :omnix-acronyms))
	 (acronyms (if acronyms-raw (string-split acronyms-raw "\n"))))
    (dolist (def acronyms)
      (let ((parts (string-split def ":")))
	(if (< (length parts) 3)
	    (message "Skipping malformed acronym: %s" def)
	  (pcase-let ((`(,key ,short . ,rest) parts))
	    (omnix-acronym--register-acronym key short (string-join rest ":")))))))
  tree)

(defun omnix-acronym--set-processor (info)
  (let ((processors (plist-get info :omnix-acronym-processor)))
    (dolist (proc processors)
      (let ((parts (string-split proc ":")))
	(if (eq (length parts) 1)
	    (add-to-list 'omnix-acronym-processor-alist `(t ,parts))
	  (add-to-list 'omnix-acronym-processor-alist
		       `(,(intern (car parts)) . ,(intern (cadr parts)))))))))

(defun omnix-acronym--set-options (tree _ info)
  (setq-local omnix-acronym-link-p (plist-get info :omnix-acronym-link))
  (omnix-acronym--set-processor info)
  tree)

(add-hook 'org-export-before-processing-hook #'omnix-acronym--clean)
(add-to-list 'org-export-filter-parse-tree-functions
	     #'omnix-acronym--register-acronyms)
(add-to-list 'org-export-filter-parse-tree-functions
	     #'omnix-acronym--set-options)

;;; Expand acronyms.
(defun omnix-acronym--create-link (type)
  "Create the various acronym link TYPEs."
  (let ((func-name (intern (format "omnix-acronym-%s-export" type)))
	(docstring (format "Export a %s link for Org files.

  PATH is the acronym's KEY.

  Link's of this type should not have a DESCRIPTION. Including a description
  will result in an error.

  FORMAT is the backend." type)))
    (defalias func-name
      (lambda (path description format)
	(:documentation docstring)

	(if description
	    (error "Acronym links do not accept descriptions"))

	(let ((processor
	       (omnix--alist-get-backend
		format omnix-acronym-processor-alist
		(alist-get t omnix-acronym-processor-alist 'plain))))
	  (pcase processor
	    ('gls (omnix-acronym--gls-processor type path format))
	    ('plain (omnix-acronym--plain-processor type path format))
	    (_ (error (format "Unknown processor \"%s\"" processor)))))))
    func-name))

(dolist (type '("gls" "acrlong" "acrshort" "acrfull"))
  (org-link-set-parameters type :export (omnix-acronym--create-link type)))

(defun omnix-acronym--gls-processor (type key _)
  "Transform a acronym link to LaTeX's acronym commands.

TYPE should be the name of a LaTeX command.
KEY is the acronym's KEY."
  (format "\\%s{%s}" type key))

(defun omnix-acronym--plain-processor (type key format)
  "Transform a acronym link to the expanded acronym.

TYPE should be the acronym macro name.
KEY is the acronym's KEY.
FORMAT is the export backend."
  (pcase type
    ("gls" (omnix-acronym--expand-auto key format))
    ("acrlong" (omnix-acronym--expand-long key format))
    ("acrfull" (omnix-acronym--expand-full key format))
    ("acrshort" (omnix-acronym--expand-short key format))))

(defun omnix-acronym--expand-short (key _)
  "Always expand the link to the KEY's short form."
  (format "%s" (omnix-acronym--get-short key)))

(defun omnix-acronym--expand-long (key _)
  "Always expand the link to the KEY's long form."
  (format "%s" (omnix-acronym--get-long key)))

(defun omnix-acronym--expand-full (key _)
  "Always expand the link to the KEY's full acronym definition."
  (format "%s (%s)" (omnix-acronym--expand-long key nil)
	  (omnix-acronym--expand-short key nil)))

(defun omnix-acronym--expand-auto (key format)
  "Insert the definition or acronym depending on if KEY has been used yet.

If this is the first time the acronym has been used with the gls link type,
insert the full definition, otherwise insert only the short form.

FORMAT is the export backend for link creation if acronym linking is set."
  (if (omnix-acronym--acronym-seen-p key)
      (omnix-acronym--maybe-link-to-anchor
       (omnix-acronym--link-name key)
       (omnix-acronym--expand-short key nil) format)
    (omnix-acronym--maybe-make-link-anchor
     (omnix-acronym--link-name key)
     (omnix-acronym--expand-full key nil) format)))

(defun omnix-acronym--link-name (key)
  "Name the link anchor for KEY."
  (format "omnix-acr-%s" key))

(defun omnix-acronym--maybe-make-link-anchor (key text format)
  "Add anchor KEY around TEXT for backend FORMAT if acronym linking is on."
  (let ((linker (omnix--alist-get-backend
		 format omnix-create-link-functions-alist)))
    (if (and omnix-acronym-link-p linker)
	(funcall linker key text) text)))

(defun omnix-acronym--maybe-link-to-anchor (key text format)
  "Link TEXT to KEY's anchor for backend FORMAT if acronym linking is on."
  (let ((linker (omnix--alist-get-backend format omnix-link-to-functions-alist)))
    (if (and omnix-acronym-link-p linker)
	(funcall linker key text) text)))

;;; Expand acronym list and acronym macros.
;; I think this is the hook needed to expand (org-export-filter-keyword-functions)

(provide 'omnix-acronym)
;;; omnix-acronym.el ends here
