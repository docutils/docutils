;; Author: David Goodger <goodger@python.org>
;; Date: $Date$
;; Copyright: This module has been placed in the public domain.
;;
;; Support code for editing reStructuredText with Emacs indented-text mode.
;; The goal is to create an integrated reStructuredText editing mode.

(defun replace-lines (fromchar tochar)
  ;; by David Goodger
  "Replace flush-left lines, consisting of multiple FROMCHAR characters,
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

(defun repeat-last-character ()
  ;; by Martin Blais
  "Fills the current line up to the length of the preceding line (if not
empty), using the last character on the current line.  If the preceding line
is empty, or if a prefix argument is provided, fill up to the fill-column.

If the current line is longer than the desired length, shave the characters
off the current line to fit the desired length.

As an added convenience, if the command is repeated immediately, the
alternative behaviour is performed."
  (interactive)
  (let* ((curcol (current-column))
	 (lbp (line-beginning-position -1))
	 (prevcol (if (= lbp 1) 
		      fill-column
		    (save-excursion
		      (forward-line -1)
		      (end-of-line)
		      (skip-chars-backward " \t" lbp)
		      (let ((cc (current-column)))
			(if (= cc 0) fill-column cc)))))
	 (rightmost-column
	  (cond (current-prefix-arg fill-column)
		((equal last-command 'repeat-last-character)
		 (if (= curcol fill-column) prevcol fill-column))
		(t (save-excursion
		     (if (= prevcol 0) fill-column prevcol))))))
    (end-of-line)
    (if (> (current-column) rightmost-column)
	;; shave characters off the end
	(delete-region (- (point)
			  (- (current-column) rightmost-column))
		       (point))
      ;; fill with last characters
      (insert-char (preceding-char)
		   (- rightmost-column (current-column))))))
  ;; It would be useful if only these characters were repeated:
  ;; =-`:.'"~^_*+#<>!$%&(),/;?@[\]{|}
  ;; Especially, empty lines shouldn't be repeated.
