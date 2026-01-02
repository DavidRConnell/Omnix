;;; omnix-acronym.el --- Org filter for managing acronyms -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

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

(require 'org)
(require 'ox)

(require 'omnix-hyperlink)
(require 'omnix--utils)
(require 'omnix--processors)
(require 'omnix--search)

;;; Public vars.
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

For this behavior set the variable to: `'((latex . gls) (t . plain))'")

;;; Defining acronyms management
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

;;; Processor management
(defvar omnix-acronym--known-processors-alist '()
  "Store all known processor groups.")

(defun omnix-acronym-create-processor (name acr acr-short acr-long acr-full
					    &optional backends setup)
  "Create a new acronym processor, NAME.

All acronym processors must implement a ACR function that defines the defualt
logic for printing an acronym, a ACR-SHORT function for printing the short form
acronym, a ACR-LONG function for print the long form and a ACR-FULL for printing
the full form.

Generally the ACR function should print the full form the first time it sees an
acronym and the short form the rest of the time.

BACKENDS, SETUP, and TEARDOWN are the standard optional values for
`omnix-processor--create'."
  (let ((processor (omnix-processor--create name backends setup))
	(acr-plist (list :acr acr
			 :acr/short acr-short
			 :acr/long acr-long
			 :acr/full acr-full)))
    (append processor acr-plist)))

(defvar omnix-acronym--plain-processor
  (omnix-acronym-create-processor "acr-plain"
				  #'omnix-acronym--acr-plain
				  #'omnix-acronym--acr-short-plain
				  #'omnix-acronym--acr-long-plain
				  #'omnix-acronym--acr-full-plain))

(defun omnix-acronym--acr-short-plain (key _)
  "Always expand the link to the KEY's short form."
  (format "%s" (omnix-acronym--get-short key)))

(defun omnix-acronym--acr-long-plain (key _)
  "Always expand the link to the KEY's long form."
  (format "%s" (omnix-acronym--get-long key)))

(defun omnix-acronym--acr-full-plain (key _)
  "Always expand the link to the KEY's full acronym definition."
  (format "%s (%s)" (omnix-acronym--acr-long-plain key nil)
	  (omnix-acronym--acr-short-plain key nil)))

(defun omnix-acronym--acr-plain (key _)
  "Insert the definition or acronym depending on if KEY has been used yet.

If this is the first time the acronym has been used with the acr link type,
insert the full definition, otherwise insert only the short form."
  (if (omnix-acronym--acronym-seen-p key)
      (omnix-acronym--acr-short-plain key nil)
    (omnix-acronym--acr-full-plain key nil)))

(defvar omnix-acronym--link-processor
  (omnix-acronym-create-processor "acr-link"
				  #'omnix-acronym--acr-link
				  #'omnix-acronym--acr-short-plain
				  #'omnix-acronym--acr-long-plain
				  #'omnix-acronym--acr-full-plain
				  (omnix--alist-keys
				   omnix-hl-link-functions-alist)
				  #'omnix-acronym--link-setup))

(defun omnix-acronym--anchor-name (key)
  "Name the link anchor for KEY."
  (format "omnix-acr-%s" key))

(defun omnix-acronym--acr-link (key backend)
  "Insert the definition or acronym depending on if KEY has been used yet.

If this is the first time the acronym has been used with the acr link type,
insert the full definition, otherwise insert only the short form.

Unlike the plain variant, this adds a BACKEND dependent link. The first instance
of the acronym will become a link anchor that the remaining future uses of the
acronym will link to."
  (if (omnix-acronym--acronym-seen-p key)
      (omnix-hl--link backend (omnix-acronym--anchor-name key)
		      (omnix-acronym--acr-short-plain key nil))
    (omnix-hl--anchor backend (omnix-acronym--anchor-name key)
		      (omnix-acronym--acr-full-plain key nil))))

(defun omnix-acronym--link-setup (info backend)
  "Set up code for the link processor.

When BACKEND is latex, modify INFO to make sure the hyperref package is used."

  (if (org-export-derived-backend-p backend 'latex)
      (omnix--add-latex-package "hyperref" info)
    info))

(defvar omnix-acronym--gls-processor
  (omnix-acronym-create-processor "acr-gls"
				  #'omnix-acronym--acr-gls
				  #'omnix-acronym--acr-short-gls
				  #'omnix-acronym--acr-long-gls
				  #'omnix-acronym--acr-full-gls
				  'latex
				  #'omnix-acronym--gls-setup))

(defun omnix-acronym--acr-gls (key _)
  "Transform a acronym KEY link to LaTeX's acronym commands.

TYPE should be the name of a LaTeX command.
KEY is the acronym's KEY."
  (format "\\gls{%s}" key))

(defun omnix-acronym--acr-short-gls (key _)
  "Transform a acronym KEY link to LaTeX's acronym commands.

TYPE should be the name of a LaTeX command.
KEY is the acronym's KEY."
  (format "\\acrshort{%s}" key))

(defun omnix-acronym--acr-long-gls (key _)
  "Transform a acronym KEY link to LaTeX's acronym commands.

TYPE should be the name of a LaTeX command.
KEY is the acronym's KEY."
  (format "\\acrlong{%s}" key))

(defun omnix-acronym--acr-full-gls (key _)
  "Transform a acronym KEY link to LaTeX's acronym commands.

TYPE should be the name of a LaTeX command.
KEY is the acronym's KEY."
  (format "\\acrfull{%s}" key))

(defun omnix-acronym--gls-setup (info _)
  "Add filters to INFO communication channel."
  (let ((acronym-definitions (plist-get info :omnix-acronyms))
	(current-headers (plist-get info :latex-header))
	(new-headers))
    (when (and acronym-definitions (not (string-empty-p acronym-definitions)))
      (setq new-headers "\\usepackage{glossaries}")
      (dolist (acr (split-string acronym-definitions "\n" t))
	(let ((parts (omnix-acronym--split-acronym-definition acr)))
	  (setq new-headers
		(concat new-headers "\n"
			(format "\\newacronym{%s}{%s}{%s}"
				(car parts) (cadr parts) (caddr parts))))))

      (setq new-headers (concat new-headers "\n\\makeglossaries"))
      (setq info (plist-put info :latex-header
			    (if current-headers
				(concat current-headers "\n" new-headers)
			      new-headers))))))

(setq omnix-acronym--known-processors-alist
      `((plain ,omnix-acronym--plain-processor)
	(link ,omnix-acronym--link-processor)
	(gls ,omnix-acronym--gls-processor)
	(fallback ,omnix-acronym--plain-processor)))

;;; Expand acronyms.
(defun omnix-acronym--create-link (type)
  "Create the various acronym link TYPEs."
  (let ((func-name (intern (format "omnix-acronym-%s-export" type)))
	(docstring (format "Export a %s link for Org files.

  PATH is the acronym's KEY.

  Link's of this type should not have a DESCRIPTION. Including a description
  will result in an error.

  BACKEND is the backend." type)))
    (defalias func-name
      (lambda (path description backend info)
	(:documentation docstring)

	(if description
	    (error "Acronym links do not accept descriptions"))

	(let* ((processor (plist-get info :omnix-acronym-processor))
	       (func (omnix-processor--get-function processor type)))
	  (if (not func)
	      (error "Function \"%s\" not defined for processor \"%s\""
		     type
		     (omnix-processor--name processor)))
	  (funcall func path backend))))
    func-name))

(defun omnix-acronym--follow-link (acronym)
  "Acronym link types' follow function for finding the ACRONYM definition."
  (omnix-search--goto-keyword-project "OMNIX_ACRONYM" (concat acronym ":")))

(defun omnix-acronym--create-link-completing-read (type)
  "Create completion functions for TYPE link."
  (let ((func-name (intern (format "omnix-acronym-%s-completing-read" type)))
	(docstring (format "Completing read for a %s link." type)))
    (defalias func-name
      (lambda (&optional _)
	(:documentation docstring)
	(let* ((candidate-alist (omnix-search--collect-keyword-project
				 "OMNIX_ACRONYM" "\\([^:]*\\):\\(.*\\)"))
	       (selection (completing-read "Acronym: " candidate-alist)))
	  (format "%s:%s" type selection))))))

(dolist (type '("acr" "acr/long" "acr/short" "acr/full"))
  (org-link-set-parameters type
			   :export (omnix-acronym--create-link type)
			   :follow #'omnix-acronym--follow-link
			   :complete
			   (omnix-acronym--create-link-completing-read type)))

;;; Parse options and set up hooks
(defun omnix-acronym--clean (&rest _)
  "Reset globals used for tracking acronym elements."
  (setq omnix-acronym--acronym-alist omnix-acronym-alist
	omnix-acronym--acronym-seen-alist '()))

(add-hook 'org-export-before-processing-functions #'omnix-acronym--clean)

;;; Set options from header keywords.
(add-to-list 'org-export-options-alist
	     '(:omnix-acronyms "OMNIX_ACRONYM" nil nil newline))
(add-to-list 'org-export-options-alist
	     '(:omnix-acronym-processor "OMNIX_ACRONYM_PROCESSOR" nil nil split))

(defun omnix-acronym--split-acronym-definition (definition)
  "Split the keyword acronym DEFINITION from key:short:long."
  (let ((parts (split-string definition ":")))
    (if (< (length parts) 3)
	(message "Skipping malformed acronym: %s" definition)
      (pcase-let ((`(,key ,short . ,rest) parts))
	(list key short (string-join rest ":"))))))

(defun omnix-acronym--register-acronyms (info)
  "Extract acronyms from INFO plist defined using OMNIX_ACRONYM keywords."
  (let* ((acronyms-raw (plist-get info :omnix-acronyms))
	 (acronyms (if acronyms-raw (split-string acronyms-raw "\n" t))))
    (dolist (def acronyms)
      (apply #'omnix-acronym--register-acronym
	     (omnix-acronym--split-acronym-definition def))))
  info)

(defun omnix-acronym--initialize-acronym-processor (info backend)
  "Initialize the acronym processor using the INFO communication channel.

Uses the export BACKEND to select the processor from the processor preference
alist.

Returns the modified INFO."
  (setq info (plist-put info :omnix-acronym-processor
			(omnix-processor--select-buffer-processor
			 backend
			 omnix-acronym-processor-alist
			 :omnix-acronym-processor
			 info
			 omnix-acronym--known-processors-alist)))
  (omnix-acronym--register-acronyms info)
  (omnix-processor--run-setup (plist-get info :omnix-acronym-processor)
			      info
			      backend))

(add-to-list 'org-export-filter-options-functions
	     #'omnix-acronym--initialize-acronym-processor)

;;; CAPF
(defun omnix-acronym-capf ()
  "Completion at point function for acronym links."
  (when (omnix-search--looking-at-link-p "acr\\(?:/[^:]*\\)?")
    (let* ((start (match-beginning 1))
	   (end (point))
	   (candidates-alist (omnix-search--collect-keyword-project
			      "OMNIX_ACRONYM" "\\([^:]*\\):\\(.*\\)")))
      (list start end
	    (mapcar #'car candidates-alist)
	    :exclusive 'no
	    :annotation-function
	    (lambda (key)
	      (let ((desc (alist-get key candidates-alist nil nil #'string=)))
		(if desc
		    (format " -- %s " desc) "")))))))

(defun omnix-acronym-setup-capf ()
  "Add the acronym CAPF to completion functions."
  (add-hook 'completion-at-point-functions #'omnix-acronym-capf nil t))

(provide 'omnix-acronym)
;;; omnix-acronym.el ends here
