;;; omnix --- Omni exporter for Org files-*- lexical-binding: t -*-

;; Copyright (C) 2025 David R. Connell

;; Author: David R. Connell <david32@dcon.addy.io>
;; Version: 0.1.0
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
;; Provides high level links and macros to help with technical writing that is
;; backend independent. When writing in Org mode, many sophisticated features,
;; such as support for text color, acronyms, and context specific
;; cross-referencing, require hardcoding backend dependent code into the org
;; file making it difficult to export to more than one backend. `OMNIX' tries
;; to provide many high level document functions directly in Emacs LISP.

;;; Code:

(require 'omnix-glossaries)

(provide 'omnix)
;;; omnix.el ends here.
