;; Authors: David Goodger <goodger@python.org>;
;;          Martin Blais
;; Date: $Date$
;; Copyright: This module has been placed in the public domain.
;;
;; Support code for editing reStructuredText with Emacs indented-text mode.
;; The goal is to create an integrated reStructuredText editing mode.
;;
;; Updates
;; -------
;;
;; 2003-02-25 (blais): updated repeat-last-character function and added
;;                     a few routines for navigating between titles.

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

(defun repeat-last-character (&optional tofill)
  "Fills the current line up to the length of the preceding line (if not
empty), using the last character on the current line.  If the preceding line is
empty, we use the fill-column.

If a prefix argument is provided, use the next line rather than the preceding
line.

If the current line is longer than the desired length, shave the characters off
the current line to fit the desired length.

As an added convenience, if the command is repeated immediately, the alternative
column is used (fill-column vs. end of previous/next line)."
  (interactive)
  (let* ((curcol (current-column))
	 (curline (+ (count-lines (point-min) (point))
		     (if (eq curcol 0) 1 0)))
	 (lbp (line-beginning-position 0))
	 (prevcol (if (and (= curline 1) (not current-prefix-arg))
		      fill-column
		    (save-excursion
		      (forward-line (if current-prefix-arg 1 -1))
		      (end-of-line)
		      (skip-chars-backward " \t" lbp)
		      (let ((cc (current-column)))
			(if (= cc 0) fill-column cc)))))
	 (rightmost-column
	  (cond (tofill fill-column)
		((equal last-command 'repeat-last-character)
		 (if (= curcol fill-column) prevcol fill-column))
		(t (save-excursion
		     (if (= prevcol 0) fill-column prevcol)))
		)) )
    (end-of-line)
    (if (> (current-column) rightmost-column)
	;; shave characters off the end
	(delete-region (- (point)
			  (- (current-column) rightmost-column))
		       (point))
      ;; fill with last characters
      (insert-char (preceding-char)
		   (- rightmost-column (current-column))))
    ))

(defun reST-title-char-p (c)
  ;; by Martin Blais
  "Returns true if the given character is a valid title char."
  (and (string-match "[-=`:\\.'\"~^_*+#<>!$%&(),/;?@\\\|]"
		     (char-to-string c)) t))

(defun reST-forward-title ()
  ;; by Martin Blais
  "Skip to the next restructured text section title."
  (interactive)
  (let* ( (newpoint
	   (save-excursion
	     (forward-char) ;; in case we're right on a title
	     (while
	       (not
		(and (re-search-forward "^[A-Za-z0-9].*[ \t]*$" nil t)
		     (reST-title-char-p (char-after (+ (point) 1)))
		     (looking-at (format "\n%c\\{%d,\\}[ \t]*$"
					 (char-after (+ (point) 1))
					 (current-column))))))
	     (beginning-of-line)
	     (point))) )
    (if newpoint (goto-char newpoint)) ))

(defun reST-backward-title ()
  ;; by Martin Blais
  "Skip to the previous restructured text section title."
  (interactive)
  (let* ( (newpoint
	   (save-excursion
	     ;;(forward-char) ;; in case we're right on a title
	     (while
	       (not
		(and (or (backward-char) t)
		     (re-search-backward "^[A-Za-z0-9].*[ \t]*$" nil t)
		     (or (end-of-line) t)
		     (reST-title-char-p (char-after (+ (point) 1)))
		     (looking-at (format "\n%c\\{%d,\\}[ \t]*$"
					 (char-after (+ (point) 1))
					 (current-column))))))
	     (beginning-of-line)
	     (point))) )
    (if newpoint (goto-char newpoint)) ))

(defun join-paragraph ()
  ;; by David Goodger
  "Join lines in current paragraph into one line, removing end-of-lines."
  (interactive)
  (save-excursion
    (backward-paragraph 1)
    (forward-char 1)
    (let ((start (point)))	; remember where we are
      (forward-paragraph 1)	; go to the end of the paragraph
      (beginning-of-line 0)	; go to the beginning of the previous line
      (while (< start (point))	; as long as we haven't passed where we started
	(delete-indentation)	; join this line to the line before
	(beginning-of-line)))))	; and go back to the beginning of the line

(defun force-fill-paragraph ()
  ;; by David Goodger
  "Fill paragraph at point, first joining the paragraph's lines into one.
This is useful for filling list item paragraphs."
  (interactive)
  (join-paragraph)
  (fill-paragraph nil))

(defun line-single-char-p ()
  "Predicate is t if the current line is composed only of a
  single repeated non-whitespace character."
  (save-excursion
    (back-to-indentation)
    (if (not (looking-at "\n"))
	(looking-at (format "[%s]+\\s-*$" (thing-at-point 'char))))
    ))

(defun reST-box-section ()
  "Adds hanging overline and underline title markers for the text
on the current line.  If the text is not indented, we indent it
by one char.

The underline character chosen is the one present in the current
underline, or '-'.

In addition, this function removes any existing overline and
underline and replaces it with the update one.  This allows you
to edit a title and then simple update the boxing around it."

  (interactive)
  (save-excursion
    (let (bc
	  ec
	  (c ?-))

      ;; Fix indentation if the line does not begin with some whitespace
      (beginning-of-line)
      (and (not (looking-at "[\t ]")) (insert " "))
      (back-to-indentation)
      (setq bc (current-column))

      ;; Remove whitespace at the end of the line
      (end-of-line)
      (delete-horizontal-space)
      (setq ec (current-column))

      ;; Remove previous line if it consists only of a single repeated character
      (save-excursion
	(forward-line -1)
	(and (line-single-char-p)
	     (kill-whole-line 1)))

      ;; Remove previous line if it consists only of a single repeated character
      (save-excursion
	(forward-line +1)
	(if (line-single-char-p)
	    (progn
	      ;; Get the previous character from the first char of the following line
	      (back-to-indentation)
	      (setq c (string-to-char (thing-at-point 'char)))
	      (kill-whole-line 1))))

      ;; Insert overline
      (beginning-of-line)
      (open-line 1)
      (move-to-column (- bc 1) t)
      (insert (make-string (+ (- ec bc) 2) c))

      ;; Insert overline
      (forward-line +2)
      (open-line 1)
      (move-to-column (- bc 1) t)
      (insert (make-string (+ (- ec bc) 2) c))

      )))

