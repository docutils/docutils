;; Author: David Goodger <goodger@python.org>
;; Date: $Date$
;; Copyright: This module has been placed in the public domain.
;;
;; Support code for editing reStructuredText with Emacs indented-text mode.
;; The goal is to integrate it into a reStructuredText editing mode.

(defun replace-lines (fromchar tochar)
  "Replace flush-left lines, consisting of multiple FROMCHAR characters, \
with equal-length lines of TOCHAR."
  (interactive "\
cSearch for flush-left lines of char: 
cand replace with char: ")
  (save-excursion
    (let* ((fromstr (string fromchar))
	   (searchre (concat "^" (regexp-quote fromstr) "+ *$"))
	   (found 0))
      (condition-case err
	  (while t
	    (search-forward-regexp searchre)
	    (setq found (1+ found))
	    (search-backward fromstr)  ;; point will be *before* last char
	    (setq p (1+ (point)))
	    (beginning-of-line)
	    (setq l (- p (point)))
	    (kill-line)
	    (insert-char tochar l))
	(search-failed
	 (message (format "%d lines replaced." found)))))))
