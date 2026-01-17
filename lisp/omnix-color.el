;;; omnix-color.el --- Color text -*- lexical-binding: t; -*-

;; Copyright (C) 2025 David R. Connell
;;
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
;; Color text with Org links. Wherever an output file type could not have color
;; (plain text backends) the fallback in `omnix-color-fallback' is used
;; instead.
;;
;; Colors can be defined with Org keyword #+OMNIX_COLOR: color:code, where
;; color is the name to use in links and code is the HTML color code.
;; If a color isn't defined, the color value will be sent directly to the
;; backend, while the code will be used if the color has been defined.
;;
;; So [[color:red][An important message]] will become
;; \textcolor{red}{An important message} for LaTeX.
;;
;; Colors can also be defined by mixing two or more colors by name using the
;; xcolor syntax of #+OMNIX_COLOR: name:color1!percent!color2, where the new
;; color can be referenced by name and will be percent color1 mixed with (100 -
;; percent) color2. When color2 is absent, white is used. Equivalently, colors
;; can be combined using "parts", like mixing paint (2 parts black to 1 part
;; white) with the syntax #+OMNIX_COLOR: name:color1,npart1;color2,npart2;...,
;; which will be converted to percentages either by the backend for LaTeX or in
;; Emacs for HTML.

;;; Code:

(require 'org)
(require 'ox)
(require 'cl-lib)

(require 'omnix--utils)
(require 'omnix--search)

(defvar omnix-color-fallback "**%s**"
  "The format string use when a backend does not support color.

The %s format-argument will be replaced by the link description
A %c argument will be replaced by the color *name* (as written in the link, not
the color code it would expand to). Both arguments are optional.

Example: <%c:%s> would expand to <red:An important message> for the link
[[color:red][An important message]].")

(defvar omnix-color-alist '()
  "An alist associating predefined color names with color codes.")

(defvar omnix-color-transcoder-alist
  `((latex . ,#'omnix-color--transcoder-latex)
    (html . ,#'omnix-color--transcoder-html)
    (t . ,#'omnix-color--transcoder-fallback))
  "An alist associating backends with functions to transcode a color link.

The transcoders should take three arguments, the color name, color code, and
the description and return a string.

Color code is a HTML code if the color has been defined in the
`omnix-color-alist' or with OMNIX_COLOR keyword options, otherwise it is the
same as the color name.")

(defvar omnix-color-re (omnix-search-keyword-re "OMNIX_COLOR"
						"\\([^:]*\\):\\(.*\\)")
  "Regex for finding color definitions.")

(defun omnix-color--transcoder-latex (name _ description)
  "Transcode the color NAME and DESCRIPTION pair to a LaTeX textcolor command."
  (format "\\textcolor{%s}{%s}" name description))

(defun omnix-color--transcoder-html (_ code description)
  "Transcode the color CODE and DESCRIPTION pair to an HTML style."
  (format "<a style=\"color:%s;\">%s</a>" code description))

(defun omnix-color--transcoder-fallback (name _ description)
  "Transcode the color NAME and DESCRIPTION pair to plain text representation."
  (replace-regexp-in-string "%\\([cs]\\)"
			    (lambda (substring)
			      (if (string= (match-string 1 substring) "c")
				  name
				description))
			    omnix-color-fallback))

(defun omnix-color--remove-prefix (prefix string)
  "Remove PREFIX from the start of STRING if it exists.

If STRING does not start with PREFIX, it is returned unmodified."
  (if (string-prefix-p prefix string)
      (cadr (split-string string prefix))
    string))

(defun omnix-color--exporter (path description backend info)
  "Color link exporter.

PATH is the name of the color, DESCRIPTION is the text to color, and BACKEND is
the export BACKEND to transcode to.

INFO is the org-exporter communication channel plist."
  (let* ((transcoder (omnix--alist-get-backend backend
					       omnix-color-transcoder-alist))
	 (color-alist (plist-get info :omnix-colors))
	 (color-code (omnix-color--mix-colors (string-trim
					       (alist-get path
							  color-alist
							  path nil #'string=))
					      color-alist)))
    (funcall transcoder path color-code description)))

(defun omnix-color--follow-link (color)
  "Color link type's follow function for finding the COLOR definition."
  (omnix-search--goto-paper color omnix-color-re))

(defun omnix-color--resolve-color (path)
  "Resolve the color PATH to an HTML HEX color."
  (let* ((color-alist (omnix-search-get-candidates omnix-color-re))
	 (colorspec (string-trim
		     (alist-get path color-alist path nil #'string=))))
    (omnix-color--mix-colors colorspec color-alist 'hex)))

(defun omnix-color--mix-colors (colorspec color-alist &optional force-hex)
  "Mix colors together if COLORSPEC is mixture recipe.

If COLORSPEC is not a mixture recipe, returns the original COLORSPEC, either
unaltered (default) or after ensuring it is a hex code when FORCE-HEX is
non-nil. COLOR-ALIST provides an alist of defined colors of the form (name .
hex-code)."
  (if (not (omnix-color--mixture-p colorspec))
      (if force-hex
	  (omnix-color--name-to-hex colorspec)
	colorspec)
    (let* ((color-parts (omnix-color--mixture-percentages colorspec))
	   (weighted-rgbs
	    (mapcar (lambda (item)
		      (mapcar (lambda (primary)
				(* (/ (plist-get item :percent) 100.0) primary))
			      (omnix-color--name-to-rgb
			       (string-trim
				(alist-get (plist-get item :color)
					   color-alist
					   (plist-get item :color)
					   nil #'string=)))))
		    color-parts)))
      (apply #'omnix-color--rgb-to-hex (apply #'cl-mapcar #'+ weighted-rgbs)))))

(defun omnix-color--name-to-hex (color)
  "Return COLOR as a hex code."
  (if (string-match-p "^#" color)
      color
    (apply #'omnix-color--rgb-to-hex (omnix-color--name-to-rgb color))))

(defun omnix-color--rgb-to-hex (red green blue)
  "Convert RED, GREEN, and BLUE components to hex.

RED, GREEN, and BLUE should be normalized so their range is [0.0, 1.0]."
  (format "#%02X%02X%02X"
	  (round (* red 255))
	  (round (* green 255))
	  (round (* blue 255))))

(defun omnix-color--name-to-rgb (color)
  "Translate COLOR to a list of RGB components.

COLOR can be a predefined name, a list of rgb, or a hex code.
Each component is normalized to be between 0.0 and 1.0.

If COLOR is already list of RGB components it is returned as is. This makes no
attempt at normalizing the list if it isn't already."
  (cond ((and (listp color) (= (length color) 3))
	 color)
	((string-match-p "^#" color)
	 (let* ((hex (substring color 1 (length color)))
		(stride (cond ((= (length hex) 3) 1)
			      ((= (length hex) 6) 2)
			      (t 4)))
		(norm (float (- (expt 16 stride) 1)))
		(idx (length hex))
		(result '()))
	   (while (> idx 0)
	     (push (/ (string-to-number
		       (substring hex (- idx stride) idx) 16)
		      norm)
		   result)
	     (setq idx (- idx stride)))
	   result))
	((stringp color)
	 (let ((rgb (alist-get color color-name-rgb-alist nil nil #'string=)))
	   (if (not rgb)
	       ;; For completion return black.
	       '(1 1 1)
	     (mapcar (lambda (component) (/ component 65535.0)) rgb))))
	(t (message "\"%s\" is not a recognized color specification." color))))

(defun omnix-color--mixture-p (colorspec)
  "Determine if COLORSPEC is a mixture of colors.

Returns true if COLORSPEC is one of the two mixture formats."
  (string-match-p (rx (or "!" ";")) colorspec))

(defun omnix-color--mixture-percentages (colorspec)
  "Create a list of plists for each color and percent in COLORSPEC.

Supports the percentage syntax (c1!p1!c2) and parts (c1,n1;c2,n2) syntax."
  (cond ((string-match-p ";" colorspec)
	 (let* ((ratios (mapcar (lambda (color)
				  (let ((parts (split-string color ",")))
				    (cons (car parts)
					  (string-to-number (cadr parts)))))
				(split-string colorspec ";")))
		(part-acc (apply #'+ 0.0 (mapcar (lambda (part)
						   (cdr part))
						 ratios))))
	   (mapcar (lambda (color)
		     (list :color (car color)
			   :percent (* 100 (/ (cdr color) part-acc))))
		   ratios)))
	((string-match-p "!" colorspec)
	 (omnix-color--parse-percentages (split-string colorspec "!")))
	(t (list (list :color colorspec :percent 100)))))

(defun omnix-color--parse-percentages (parts)
  "Parse a list of color PARTS to a flast list of color plists.

Color plists are of the form (:color code :percent value)."
  (when (< (length parts) 3)
    (setq parts (append parts '("white"))))

  (if (not parts)
      nil
    (let* ((c1 (car parts))
	   (w1 (string-to-number (cadr parts)))
	   (c2 (caddr parts))
	   (current-mix
	    ;; If c1 is already a color plist have to multiple each component
	    ;; by c1's weight to get a flat list of color plists.
	    (if (listp c1)
		(append (mapcar (lambda (item)
				  (list :color (plist-get item :color)
					:percent (* (/ w1 100.0)
						    (plist-get item :percent))))
				c1)
			(list (list :color c2 :percent (- 100.0 w1))))
	      (list (list :color c1 :percent w1)
		    (list :color c2 :percent (- 100.0 w1)))))
	   (rest (cdddr parts)))
      (if rest
	  (omnix-color--parse-percentages (cons current-mix rest))
	current-mix))))

(defun omnix-color--link-face (color)
  "Set the color link's foreground to COLOR."
  (let ((hex-code (omnix-color--resolve-color color)))
    (if (and hex-code (color-supported-p hex-code))
	`(:inherit org-link :foreground ,hex-code)
      'org-link)))

(org-link-set-parameters "color"
			 :export #'omnix-color--exporter
			 :follow #'omnix-color--follow-link
			 :face #'omnix-color--link-face)

;; Handle options
(add-to-list 'org-export-options-alist
	     '(:omnix-colors "OMNIX_COLOR" nil nil split))

(defun omnix-color--register-colors (info)
  "Extract colors from INFO plist defined using OMNIX_COLOR keywords."
  (let* ((color-defs (plist-get info :omnix-colors))
	 (color-alist omnix-color-alist))
    (dolist (def color-defs)
      (let ((parts (split-string def ":")))
	(if (not (eq (length parts) 2))
	    (message "Malformed color definition \"%s\"; skipping." def)
	  (setq color-alist
		(append color-alist (list (cons (car parts) (cadr parts))))))))
    (setq info (plist-put info :omnix-colors color-alist)))
  info)

(defun omnix-color--setup-latex (info)
  "Use the INFO plist to create the LaTeX preamble.

Adds the xcolor package and transcodes the keyword OMNIX_COLOR definitions to
xcolor's definecolor commands."
  (if (plist-get info :omnix-plain)
      info ; Do not use package xcolor when running in plain mode.
    (let ((color-alist (plist-get info :omnix-colors))
	  (current-headers (plist-get info :latex-header))
	  (new-headers "\\usepackage{xcolor}"))
      (dolist (color-pair color-alist)
	(let ((name (car color-pair))
	      (code (omnix-color--remove-prefix "#" (cdr color-pair))))
	  (setq new-headers
		(concat new-headers "\n"
			(cond
			 ((string-match-p "!" code)
			  (format "\\colorlet{%s}{%s}" name code))
			 ((string-match-p ";" code)
			  (format "\\colorlet{%s}{rgb:%s}" name code))
			 (t (format "\\definecolor{%s}{HTML}{%s}" name code)))))))

      (setq info (plist-put info :latex-header
			    (if current-headers
				(concat current-headers "\n" new-headers)
			      new-headers))))))

(defun omnix-color--setup (info backend)
  "Register the colors found in the options of the INFO plist.

And maybe set up the LaTeX preamble if using a LaTeX based BACKEND."
  (omnix-color--register-colors info)
  (if (org-export-derived-backend-p backend 'latex)
      (omnix-color--setup-latex info)
    info))

(add-to-list 'org-export-filter-options-functions #'omnix-color--setup)

;;; CAPF
(defun omnix-color-capf ()
  "Completion at point function for color links."
  (when (omnix-search--looking-at-link-p "color\\(?:/[^:]*\\)?")
    (let* ((start (match-beginning 1))
	   (end (point))
	   (candidates-alist (omnix-search-get-candidates omnix-color-re)))
      (list start end
	    (mapcar #'car candidates-alist)
	    :exclusive 'no
	    :annotation-function
	    (lambda (key)
	      (let ((hex-code
		     (substring-no-properties
		      (omnix-color--resolve-color key)))
		    (desc (alist-get key candidates-alist nil nil #'string=)))
		(if (and hex-code (color-supported-p hex-code))
		    (format "  %s" (propertize desc 'face
					       `(:foreground ,hex-code)))
		  "")))))))

(defun omnix-color-setup-capf ()
  "Add the color CAPF to completion functions."
  (add-hook 'completion-at-point-functions #'omnix-color-capf nil t))

(provide 'omnix-color)
;;; omnix-color.el ends here
