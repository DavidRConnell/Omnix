;;; omnix--processors --- Processor utilities -*- lexical-binding: t; -*-

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
;; Defines a high level interface for processors and some generic utilities for
;; choosing a processor for the current backend.
;;
;; The create processor function should be treated as a "base class" that
;; implements a common processor protocol related to processor choice within
;; submodules. Each submodule that needs protocols is expected to define it's
;; own creation functions around the base function defined here.
;;
;; Processors are plists of functions needed to perform the submodules text
;; transformations. The base protocol defines backends the protocol can handle
;; and optional setup / teardown functions to be run before and after
;; processing. Multiple processors can be grouped together in a single name
;; with backend specific implementations (for example, acronyms has a "link"
;; group which adds hyperlinking to acronyms, requiring different syntax for
;; different backends). Each processor is stored in a submodule specific alist
;; of known processors associating a processor group with 1 or more processor
;; implementations. To select a processor, an alist associating backends with
;; processor names is used to determine the preferred processor, the known
;; processor alist is then scanned to find the first processor that can handle
;; the backend, if none is found the fallback is returned instead. Generally,
;; only one processor per group should handle a given backend but in cases
;; where a processor group has a generic (backend independent) processor, care
;; should be taken to ensure it is at the end of the list for that group.
;;
;; When possible, all submodules using processors should define a fallback
;; processor (typically "plain") that is based on pure text transformations and
;; is therefore independent on the export backend. When selecting a processor,
;; if the processor initially chosen based on the processor alist is not
;; compatible with the backend, the fallback processor will be given instead. A
;; "plain" variant may not always be feasible (for example if processors is
;; used in the color submodule, it would not be possible to implement pure text
;; color). In these cases it should still be preferred to define a default
;; behavior through a "plain" processor (such as emphasizing the text in lieu
;; of font color).

;;; Code:

(require 'ox)
(require 'omnix--utils)

(defun omnix-processor--create (&optional backends setup teardown)
  "Create processor that handles BACKENDS.

When BACKENDS is left nil, the processor is assumed to not be specific to a
backend and therefore work for any. BACKENDS can be nil, a backend symbol or a
list of symbols.

Extra SETUP and TEARDOWN functions can be added to the processor. Use of these
functions is dependent on the specific submodule but should be called at the
beginning and end of the export process respectively to allow adding (and
cleaning up) any org-export hooks the processor needs."
  (let ((backends (if (and backends (symbolp backends))
		      (list backends)
		    backends)))
    (list :backends backends :setup setup :teardown teardown)))

(defun omnix-processor--select (format preference-alist known-processors)
  "Given the export FORMAT and user's PREFERENCE-ALIST, choose a processor.

The PREFERENCE-ALIST should associate export backends with a processor group.
Using the value associated with t as the default for any backends without an
explicit preferenc.

KNOWN-PROCESSORS is an alist associating processor groups with a list of
processor plist. Each processor in the selected group will be checked for
compatibility, the first found is returned. If there are no compatible
processors in the group, the processor associated with fallback is returned. An
error is thrown if the fallback group is needed but has not been defined in the
PREFERENCE-ALIST."
  (let* ((preference (omnix--alist-get-backend format preference-alist))
	 (group (alist-get preference known-processors))
	 (selection (cl-remove-if
		     (lambda (proc)
		       (not (omnix-processor--compatible-p format proc)))
		     group)))
    (if (> (length selection) 0)
	(car selection)
      (omnix-processor--get-fallback known-processors))))

(defun omnix-processor--compatible-p (format processor)
  "Decide if the PROCESSOR is compatible with FORMAT."
  (let ((compatible-with (plist-get processor :backends)))
    (if (not compatible-with)
	;; Admittedly odd behavior, but the logic is if the processor doesn't
	;; explicitly state compatible backends it is assumed to work with any
	;; backend.
	t
      (> (length (cl-remove-if
		  (lambda (test) (not (org-export-derived-backend-p format test)))
		  compatible-with))
	 0))))

(defun omnix-processor--get-fallback (known-processors)
  "Retrieve the fallback processor from KNOWN-PROCESSORS."
  (let ((fallback (car (alist-get 'fallback known-processors))))
    (if (not fallback)
	(error "No fallback processor given; cannot handle this backend")
      fallback)))

(provide 'omnix--processors)
;;; omnix--processors.el ends here
