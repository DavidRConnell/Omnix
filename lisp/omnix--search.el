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
;;
;; Uses INCLUDE keywords to determine the current paper and it's Org file
;; dependencies. This enables text completion to be aware of multiple
;; org-files. In generating a tree, we assume each paper will only have one Org
;; file that has a TITLE keyword. A single Org file could be reused in multiple
;; papers but the paper switching mechanism will assume we remain in the last
;; paper unless we switch to an Org buffer that is not in the last seen paper.
;; So if an Org file is reused in multiple papers, Omnix will the current paper
;; is whichever paper was already being modified (or whichever happens to be
;; the first in the paper cache with that file if we weren't already in an
;; appropriate paper).
;;
;; Paper structure management is based on caches. Caches are updated after
;; switching to an Org file outside of the current project. Additionally, the
;; cache associated with the current buffer's file is always updated when a
;; completion command is run. This should keep caches up-to-date under the
;; assumption that a paper's Org file is only being modified with this Emacs
;; instance. If an Org file has been modified externally, it's cache may become
;; stale. You can either call `omnix-search-clear-caches' or switch in and out
;; of the paper to update the caches.

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

(defun omnix-search-keyword-re (name groups)
  "Create a regex for finding keyword NAME and collect regex GROUPS.

See also `omnix-search-property-re'."
  (format (rx bol (* whitespace) "#+%s:" (* whitespace) "%s")
	  name groups))

(defun omnix-search-property-re (name &rest groups)
  "Create a regex for finding drawer property NAME and collect regex GROUPS.

See also `omnix-search-keyword-re'.")

(defvar omnix-search-include-re
  (omnix-search-keyword-re "INCLUDE" "\\(.*\\.org$\\)")
  "Regular expression for matching included Org files.")

;; Paper management
(defvar omnix-search--current-paper nil
  "The parent Org file for the current paper.")

(defvar omnix-search--re-caches '()
  "An alist of caches for storing search results across files.

 Associates symbols naming different caches with their cache, which in turn is
 an alist. Caches associate file names with metadata lists.

See function `omnix-search--last-cache-updates' for tracking last update.")

(defun omnix-search--get-cache (pattern)
  "Get the cache associated with PATTERN."
  (alist-get pattern omnix-search--re-caches '() nil #'string=))

(defun omnix-search--set-cache (pattern new)
  "Set the cache associated with PATTERN to NEW."
  (setf (alist-get pattern omnix-search--re-caches '() nil #'string=) new))

(defun omnix-search--paper-files ()
  "Provide the list of Org files included in the current paper."
  (omnix-search--get-cache omnix-search-include-re))

(defvar omnix-search--last-cache-updates '()
  "An alist of the last time caches were updated.

Uses the same keys as `omnix-search--re-caches'.")

(defun omnix-search--last-cache-updates (pattern)
  "Provide list of last scan times for PATTERN's cache."
  (alist-get pattern omnix-search--last-cache-updates '() nil #'string=))

(defun omnix-search--maybe-switch-paper ()
  "Switch the current paper to PAPER.

This clears and generates caches for the new PAPER and stores the new PAPER's
Org dependency structure."
  (when (or (not (omnix-search--paper-files))
	    (omnix-search--changed-paper-p))
    (omnix-search-clear-caches)
    (omnix-search--set-paper)))

(defun omnix-search--changed-paper-p ()
  "Determine if current buffer is a member of last known paper.

Returns nil if buffer is not tracking an Org file."
  (and (eq major-mode 'org-mode)
       (not (member (buffer-file-name)
		    (omnix-search--paper-files)))))

(defun omnix-search-clear-caches ()
  "Delete all stored caches to force regenerating them."
  (interactive)
  (setq omnix-search--last-cache-updates '())
  (setq omnix-search--re-caches '()))

(defun omnix-search--set-paper ()
  "Determine the current paper.

Selects a paper based on the current buffer from those in the current root
directory and uses that to update `omnix-search--re-caches'"
  (let* ((project-org-files (directory-files-recursively
			     (project-root (project-current)) "\\.org$"))
	 (parent "")
	 (include-list
	  (mapcar (lambda (ls)
		    (setq parent (file-name-directory (car ls)))
		    (cons (car ls)
			  (mapcar (lambda (f)
				    (omnix-search--absolute-path f parent))
				  (cdr ls))))
		  (omnix-search--collect-files omnix-search-include-re
					       project-org-files))))
    (omnix-search--select-paper-files (buffer-file-name) include-list)))

(defun omnix-search--absolute-path (path parent)
  "Convert a relative PATH to an absolute path under PARENT."
  (unless (file-name-absolute-p path)
    (when (and (eq (aref path 0) ?.) (eq (aref path 1) ?/))
      (setq path (substring path 2 (length path))))
    (setq path (expand-file-name path parent)))
  path)

(defun omnix-search--select-paper-files (file file-list)
  "Create an Org file include tree related to FILE.

Using the project wide file inclusion list FILE-LIST, finds all files in FILE's
paper in the form of a tree.

First pops up until at a root-file, then maps over files included by that
file recursively to end up with a tree of lists where each list is a cons pair
of file . included file and each included file is of the same form.

Unexpectedly, there are two ways a file can be considered top-level. The
obvious way, no file includes it and secondly, if it is included by multiple
files. This is deemed top-level since it would not be safe to use definitions
above a file that has been included by multiple files (i.e. if one of two files
that includes the file creates a figure and the file tries to reference that
figure, exporting the other paper would fail with an unknown figure)."
  (let ((root-file (omnix-search--root file file-list)))
    (omnix-search--set-cache
     omnix-search-include-re
     (flatten-tree (omnix-search--included-files root-file file-list))))

  (setf (alist-get omnix-search-include-re
		   omnix-search--last-cache-updates
		   nil nil #'string=)
	(let ((now (time-convert (current-time) 'integer)))
	  (mapcar (lambda (f) (cons f now))
		  (omnix-search--paper-files)))))

(defun omnix-search--root (file file-list)
  "Find the root-file of the paper FILE is included in.

FILE-LIST is a list of all files in the project and which files they include.

The root-file is a file that is included by zero or multiple files (see
`omnix-search--select-paper-files')"
  (let ((above (remq nil
		     (mapcar (lambda (included)
			       (if (member file (cdr included))
				   (car included)
				 nil))
			     file-list))))
    (if (eq (length above) 1)
	(omnix-search--root (car above) file-list)
      file)))

(defun omnix-search--included-files (file file-list)
  "Recursively collect all files included by FILE.

FILE-LIST is the list of all files in the project and which files they
include.

Returns a list of pairs (file . included) where included is of the same form."
  (let ((included (alist-get file file-list '() nil #'string=)))
    (cons file (mapcar (lambda (f)
			 (omnix-search--included-files f file-list))
		       included))))

(defun omnix-search-get-candidates (pattern)
  "Get a list of candidates for the cache associated with symbol CACHE.

Combines all cached candidates across a paper's Org files, searches the current
buffer using PATTERN, and updates the buffer's file's cache with the results of
the new search.

If no cache exists, PATTERN is used to search all files in paper and populate
the cache."
  (apply #'append (mapcar #'cdr (omnix-search--collect-paper pattern))))

;; Collection
(defun omnix-search--collect-paper (pattern)
  "Search for PATTERN across current paper and add to cache.

Only searches files that have been modified since last collection and current
buffer. Current buffer is always scanned since it may have been modified by not
saved.

Returns the list of pattern matches (same as the contents of the cache)"
  (omnix-search--maybe-update-paper-files)

  ;; In case the buffer has been modified but not saved.
  (when (eq major-mode 'org-mode)
    (setf (alist-get (buffer-file-name)
		     (alist-get pattern
				omnix-search--re-caches
				nil nil #'string=)
		     nil nil #'string=)
	  (omnix-search--collect-buffer pattern)

	  (alist-get (buffer-file-name)
		     (alist-get pattern
				omnix-search--last-cache-updates
				nil nil #'string=)
		     nil nil #'string=)
	  (time-convert (current-time) 'integer)))

  (let ((modified-files (omnix-search--modified-files pattern))
	(cache (omnix-search--get-cache pattern))
	(scan-times (omnix-search--last-cache-updates pattern))
	(now (time-convert (current-time) 'integer)))
    (dolist (m (omnix-search--collect-files pattern modified-files))
      (setf (alist-get (car m)
		       (alist-get pattern
				  omnix-search--last-cache-updates
				  nil nil #'string=)
		       nil nil #'string=)
	    now
	    (alist-get (car m)
		       (alist-get pattern
				  omnix-search--re-caches
				  nil nil #'string=)
		       nil nil #'string=)
	    (cdr m))))

  (omnix-search--get-cache pattern))

(defun omnix-search--modified-files (pattern)
  "Return a list of paper files that have been modified since last scan.

Uses cache for PATTERN."
  (let ((now (time-convert (current-time) 'integer))
	(last-updates (omnix-search--last-cache-updates pattern)))
    (remq nil
	  (mapcar (lambda (file)
		    (let ((mtime (time-convert
				  (file-attribute-modification-time
				   (file-attributes file))
				  'integer))
			  (last-update (alist-get file
						  last-updates
						  0 nil #'string=)))
		      (if (> mtime last-update)
			  file
			nil)))
		  (omnix-search--paper-files)))))

(defun omnix-search--maybe-update-paper-files ()
  "Recheck included Org files in modified paper files.

If any of the paper's org files have new includes, bust cache and recreate.
This could be better optimized by updating in place instead of clearing and
recreating the caches but I suspect this will be fast enough (especially when
using RG or grep) and rare enough to not be worth the extra effort."
  (omnix-search--maybe-switch-paper)

  (let* ((modified (omnix-search--modified-files omnix-search-include-re))
	 (cache (omnix-search--get-cache omnix-search-include-re))
	 (new-includes (omnix-search--collect-files omnix-search-include-re
						    modified))
	 (last-updates (omnix-search--last-cache-updates
			omnix-search-include-re))
	 (file "")
	 (changed nil)
	 (now (time-convert (current-time) 'integer)))

    (setf (alist-get (buffer-file-name) new-includes nil nil #'string=)
	  (omnix-search--collect-buffer omnix-search-include-re))

    (while (and modified (not changed))
      (setq file (car modified)
	    modified (cdr modified))
      (when (omnix-search--includes-diff-p
	     (alist-get file new-includes nil nil #'string=)
	     (alist-get file cache nil nil #'string=))
	(setf (alist-get file last-updates nil nil #'string=) now)
	(setq changed t)))

    (when changed
      (omnix-search-clear-caches)
      (omnix-search--set-paper))))

(defun omnix-search--includes-diff-p (ls1 ls2)
  "Compare include lists LS1 and LS2 to determine if they differ.

Lists in different order will be considered different."
  (cond ((and (not ls1) (not ls2)) nil)
	((not (string= (car ls1) (car ls2))) t)
	(t (omnix-search--includes-diff-p (cdr ls1) (cdr ls2)))))

(defun omnix-search--collect-files (pattern files)
  "Search for PATTERN across FILES.

Returns a list of con cells where the car of each element is the file name and
the cdr is the list of matches."
  (if (member omnix-search-program '(ripgrep grep))
      (omnix-search--collect-files-external omnix-search-program pattern files)
    (omnix-search--collect-files-elisp)))

(defun omnix-search--collect-files-external (program pattern files)
  "External variant of searching PATTERN across all FILES.

Use external PROGRAM to search files."
  (when files
    (let* ((cmd-template (pcase program
			   ('ripgrep "rg --no-line-number --color=never '%s' %s")
			   ('grep "grep --color=never -E '%s' %s")))
	   (cmd (format cmd-template
			(string-replace "\\)" ")" (string-replace "\\(" "(" pattern))
			(string-join files " ")))
	   (parent (file-name-directory (buffer-file-name)))
	   (results (mapcar (lambda (f) (cons f nil)) files)))

      (with-temp-buffer
	(call-process-shell-command cmd nil t)
	(goto-char (point-min))

	(while (not (eobp))
	  (let ((line-end (line-end-position))
		(filename (if (> (length files) 1)
			      (progn
				(search-forward ":" line-end t)
				(omnix-search--absolute-path
				 (buffer-substring-no-properties
				  (line-beginning-position) (- (point) 1))
				 parent))
			    (car files))))
	    ;; Ensure patterns that expect match to start at BOL work.
	    (delete-char (- (line-beginning-position) (point)))
	    (when (re-search-forward pattern line-end t)
	      (push (if (match-string 2)
			(cons (match-string-no-properties 1)
			      (match-string-no-properties 2))
		      (match-string-no-properties 1))
		    (alist-get filename results nil nil #'string=))))
	  (forward-line 1)))

      (if (match-string 2)
	  (dolist (file results)
	    (setf (alist-get (car file) results nil nil #'string=)
		  (omnix-search--pad-whitespace
		   (alist-get (car file) results nil nil #'string=)))))
      results)))

(defun omnix-search--collect-files-elisp (pattern files)
  "Emacs-lisp variant of searching PATTERN across all FILES."
  (let (results)
    (dolist (f files results)
      (push (cons f (with-temp-buffer
		      (insert-file-contents f)
		      (omnix-search--collect-buffer pattern))) results))
    results))

(defun omnix-search--collect-buffer (pattern)
  "Collect all PATTERN matches in the current buffer.

PATTERN should have one or two capturing groups in it. If PATTERN has one
group, return a list of the first capturing group. If two groups, returns an
alist associating group 1 to group 2."
  (let ((results '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(if (match-string 2)
	    (push (cons (match-string-no-properties 1)
			(match-string-no-properties 2))
		  results)
	  (push (match-string-no-properties 1) results))))
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

(defun omnix-search--goto-project (name pattern)
  "Find and goto first instance of NAME in Org project.

Searches project for PATTERN. To make use of caches, PATTERN must be the same
used to search the project in previous searches.

If NAME is found, jump to that point. If point is in another file,
`find-file' then jump to point."

  (let ((candidates (omnix-search--collect-paper pattern))
	(foundp nil)
	(file-matches nil)
	(file ""))
    (while (and candidates (not foundp))
      (setq file-matches (car candidates)
	    candidates (cdr candidates))
      (when (member name (mapcar #'car (cdr file-matches)))
	(setq foundp t
	      file (car file-matches))))

    (when foundp
      (unless (string= file (buffer-file-name))
	(find-file file))

      (goto-char (point-min))
      (while (and (search-forward-regexp pattern)
		  (not (string= (match-string 1) name))))
      (goto-char (match-beginning 1)))))

(defun omnix-search--looking-at-link-p (type)
  "Test if point is currently in an omnix link of TYPE.

For example, returns non-nil when in an `org-mode' buffer and point is at
[[acr:|, if TYPE is acr.

If non-nil, `match-string' will be set."
  (and (eq major-mode 'org-mode)
       (looking-back (format (rx "[[%s:" (group (* (not "]")))) type)
		     (line-beginning-position))))

(provide 'omnix--search)
;;; omnix--search.el ends here
