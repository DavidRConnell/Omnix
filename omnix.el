;;; omnix --- Omni exporter for Org files-*- lexical-binding: t -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Version: 0.1.0-alpha
;; URL: https://github.com/davidrconnell/omnix
;; Keywords: outlines, text
;; Package-Requires: ((org "9.4") (batch-ox "0.1"))

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
;; Provides high level links and macros to help with technical writing in a
;; backend independent manner. When writing in Org mode, many sophisticated
;; features, such as support for text color, acronyms, and context specific
;; cross-referencing, require hardcoding backend dependent code into the org
;; file making it difficult to export to more than one backend. Omnix tries
;; to provide many high level document functions directly in Emacs LISP.
;;
;; Design Principle:
;;   The general design principle is to define multiple processors for most
;;   tasks. Generally, there will be a "plain" processor that depends entirely
;;   on plain text transformations, while there may also be more sophisticated
;;   processors, like "link", that will depend on features not available
;;   everywhere. When possible, these processors will abstract out the backend
;;   dependent logic to produce consistent output through different exporters.
;;   Some processors will be specific to a backend, often LaTeX, and will defer
;;   all processing to a backend specific package.
;;
;;   By default, the "plain" processor will be used but the processor can be
;;   changed on a per backend basis using either the global alist variables or
;;   keyword options in the Org mode buffer.
;;
;; Usage:
;;   Due to one of the goals being to produce minimal tex files that can be
;;   fully converted through other methods (such as submitting a tex file to
;;   someone else who will use to generate other files), Omnix is designed to
;;   be loaded a-la-cart to prevent extra LaTeX packages from ending up in the
;;   preamble when not needed.
;;
;;   Modules that are minimally invasive (at least with the default settings)
;;   will be loaded with (require 'omnix), whereas modules that
;;   indiscriminately add packages to tex files (like `omnix-color') will
;;   require manual requiring.
;;
;;   To load all modules you can call the `omnix-global' function.
;;
;;  Modules:
;;    Omnix's features are exposed through different self-contained modules.
;;    Every public module should provide one high level feature and should work
;;    independent of which other modules have been loaded.
;;
;;    Current modules:
;;      `omnix-acronym' - Manages acronym definitions.
;;      `omnix-sansext' - Links to select backend preferred image types.
;;      `omnix-color' - Text coloring.

;;; Code:

(require 'omnix-acronym)
(require 'omnix-sansext)

(defun omnix-global ()
  "Load all omnix modules.

By default requiring Omnix only loads modules that don't impact tex outputs
unless they are actually used. (Such as acronym, which by default will not
impact the tex file produced unless a acronym is defined.)

Other modules can be required manually or calling this function will load all
public modules."
  (require 'omnix-color))

(provide 'omnix)
;;; omnix.el ends here.
