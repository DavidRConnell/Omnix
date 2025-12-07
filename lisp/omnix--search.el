;;; omnix--search.el --- Find patterns across an Org project -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>

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
;; Provides reusable commands for searching regexp patterns in a file or over
;; the entire Org project using an available search program.

;;; Code:

(require 'project)
(require 'org)

(defcustom omnix-search-program
  (cond
   ((executable-find "rg") 'ripgrep)
   ((executable-find "grep") 'grep)
   (t 'elisp))
  "The tool omnix uses to search for regexp patterns.
Defaults to ripgrep or grep when available and falls back on elisp."
  :type '(choice (const :tag "Ripgrep" ripgrep)
		 (const :tag "Grep" grep)
		 (const :tag "Native Elisp" elisp))
  :group 'omnix)

(defun omnix-search--goto-buffer (pattern)
  "Search the open buffer for PATTERN and goto its point.

If PATTERN is not found, returns NIL otherwise returns the position of the
PATTERN (and jumps there)."
  (let ((pos (save-excursion
	       (goto-char (point-min))
	       (re-search-forward pattern nil t))))
    (if pos (progn
	      (org-mark-ring-push)
	      (goto-char pos)))
    pos))

(defun omnix-search--collect-buffer (pattern)
  "Collect all PATTERN matches in the current buffer.

PATTERN should have one or two capturing groups in it. If PATTERN has one
group, return a list of the first capturing group. If two groups, returns a
alist associating group 1 to group 2."
  (let ((results '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(if (match-string 2)
	    (push (cons (substring-no-properties (match-string 1))
			(substring-no-properties (match-string 2)))
		  results)
	  (push (substring-no-properties (match-string 1)) results))))
    (if (match-string 2)
	(omnix-search--pad-whitespace results)
      results)))

(defun omnix-search--pad-whitespace (collection)
  "Pad the whitespace of the description (cdr) of COLLECTION alist.

When used with completion backends, this will ensure all annotations are
aligned."
  (let ((max-len 0))
    (dolist (pair collection)
      (if (> (length (cdr pair)) max-len)
	  (setq max-len (length (cdr pair)))))

    (dolist (pair collection)
      (setcdr pair (string-pad (cdr pair) max-len))))
  collection)

(defun omnix-search--goto-file (pattern file)
  "Search FILE for PATTERN and goto its point.

If PATTERN is not found, returns NIL otherwise returns the position of the
PATTERN (and jumps there)."
  ;; TODO: use `omnix-search-program' ot find position
  (let ((pos (with-temp-buffer
	       (insert-file-contents file)
	       (omnix-search--goto-buffer pattern))))
    (if pos
	(progn
	  (find-file file)
	  (goto-char pos)))
    pos))

(defvar keyword-re "^[ \t]*#\\+%s:[ \t]*%s"
  "Regex pattern template to use when searching for keywords.")

(defun omnix-search--goto-keyword-project (keyword value)
  "Search project for header KEYWORD with VALUE and jump to it.

Useful for link follow functions where the link's value is defined as in a
keyword."
  (let ((pattern (format keyword-re keyword value)))
    (omnix-search--goto-project pattern)))

(defun omnix-search--goto-project (pattern)
  "Find and goto first instance of PATTERN in Org project.

Searches project for pattern starting with the current buffer then looking
through the project dependency graph.

The dependency tree is formed by finding all Org files in the `project-root'
and searching for INCLUDE macros to find all Org files related to the current
buffer. It then searches bottom up (from buffer than Org files that directly
include buffer's file, followed by files that include files that include
buffer's file, etc.)

If PATTERN is found, jump to that point. If point is in another file,
`find-file' then jump to point.

Returns the position of PATTERN if PATTERN found otherwise NIL."

  ;; TODO
  (unless (omnix-search--goto-buffer pattern)
    (message "Not found in buffer")))

(defun omnix-search--collect-keyword-project (keyword pattern)
  "Collect the values of KEYWORD.

PATTERN should be a regexp with 1 or 2 capturing groups describing the value."
  (omnix-search--collect-project (format keyword-re keyword pattern)))

(defun omnix-search--collect-project (pattern)
  "Collect all instances of PATTERN in current `org-mode' project.

TEMP: just collect within buffer."
  (omnix-search--collect-buffer pattern))

(defun omnix-search--looking-at-link-p (type)
  "Test if point is currently in an omnix link of TYPE.

For example, returns non-nil when in an `org-mode' buffer and point is at
[[acr:|, if TYPE is acr.

If non-nil, `match-string' will be set."
  (and (eq major-mode 'org-mode)
       (looking-back (format "\\[\\[%s:\\([^]]*\\)" type)
		     (line-beginning-position))))

(provide 'omnix--search)
;;; omnix--search.el ends here
