;;; rst-mode.el --- Goodies to automate converting reST documents to HTML.

;; Copyright 2003 Martin Blais <blais@iro.umontreal.ca>
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This package provides a few functions and variables that can help in
;; automating converting reST documents to HTML from within emacs.  You could
;; use a makefile to do this, of use the compile command that this package
;; provides.

;; You can also bind a command to automate converting to HTML:
;; (defun user-rst-mode-hook ()
;;   (local-set-key [(control c)(?9)] 'rst-html-compile))
;; (add-hook 'rst-mode-hook 'user-rst-mode-hook)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup rst-html nil 
  "Settings for conversion to HTML available by \\[rst-html-compile]. Use of
this functionality is discouraged. Get a proper `Makefile' instead."
  :group 'rst
  :version "21.1")

(defcustom rst-html-command "docutils_html"
  "Command to convert an reST file to HTML."
  :group 'rst-html
  :type '(string))

(defcustom rst-html-stylesheet ""
  "Stylesheet for reST to HTML conversion. Empty for no special stylesheet."
  :group 'rst-html
  :type '(string))

(defcustom rst-html-options ""
  "Local file options for reST to HTML conversion.
Stylesheets are set by an own option."
  :group 'rst-html
  :type '(string))

(defcustom rst-html-extension ".html"
  "Extension for HTML output file."
  :group 'rst-html
  :type '(string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion to HTML

(defun rst-html-compile ()
  "Compile command to convert reST document into HTML."
  (interactive)
  (let* ((bufname (file-name-nondirectory buffer-file-name))
	 (outname (file-name-sans-extension bufname))
	 (ssheet
	  (or (and (not (zerop (length rst-html-stylesheet)))
		   (concat "--stylesheet=\"" rst-html-stylesheet "\""))
	      "")))
    (set (make-local-variable 'compile-command)
	 (mapconcat 'identity
		    (list rst-html-command
			  ssheet rst-html-options
			  bufname (concat outname rst-html-extension))
		    " "))
    (if (or compilation-read-command current-prefix-arg)
	(call-interactively 'compile)
      (compile compile-command))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; rst-mode.el ends here
