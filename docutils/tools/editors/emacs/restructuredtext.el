;; Authors: David Goodger <goodger@python.org>,
;;          Martin Blais <blais@furius.ca>
;; Date: $Date$
;; Copyright: This module has been placed in the public domain.
;;
;; Support code for editing reStructuredText with Emacs indented-text mode.
;; The goal is to create an integrated reStructuredText editing mode.
;;
;; Installation instructions
;; -------------------------
;;
;; Add this line to your .emacs file::
;;
;;   (require 'restructuredtext)
;;
;; You should bind the versatile sectioning command to some key in the text-mode
;; hook. Something like this::
;;
;;   (defun user-rst-mode-hook ()
;;     (local-set-key [(control ?=)] 'rest-adjust-section-title)
;;     )
;;   (add-hook 'text-mode-hook 'user-rst-mode-hook)
;;
;; Other specialized and more generic functions are also available.
;; Note that C-= is a good binding, since it allows you to specify a negative
;; arg easily with C-- C-= (easy to type), as well as ordinary prefix arg with
;; C-u C-=.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Filter function.

(if (not (fboundp 'filter))
    (defun filter (pred list)
      "Returns a list of all the elements fulfilling the pred requirement (that
is for which (pred elem) is true)"
      (if list
          (let ((head (car list))
                (tail (filter pred (cdr list))))
            (if (funcall pred head)
                (cons head tail)
              tail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generic text functions that are more convenient than the defaults.
;;

(defun replace-lines (fromchar tochar)
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

(defun join-paragraph ()
  "Join lines in current paragraph into one line, removing end-of-lines."
  (interactive)
  (let ((fill-column 65000)) ; some big number
    (call-interactively 'fill-paragraph)))

(defun force-fill-paragraph ()
  "Fill paragraph at point, first joining the paragraph's lines into one.
This is useful for filling list item paragraphs."
  (interactive)
  (join-paragraph)
  (fill-paragraph nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following functions implement a smart automatic title sectioning feature.
;; The idea is that with the cursor sitting on a section title, we try to get as
;; much information from context and do the best thing. This function can be
;; invoked many time and/or with prefix argument to rotate between the various
;; options.
;;
;; There are two styles of sectioning:
;;
;; 1. simple-underline, e.g.      |Some Title
;;                                |----------
;;
;; 2. overline-and-underline, e.g.  |------------
;;                                  | Some Title
;;                                  |------------
;;
;; Some notes:
;;
;; - the underlining character that is used depends on context. The file is
;;   scanned to find other sections and an appropriate character is selected.
;;   If the function is invoked on a section that is complete, the character
;;   is rotated among the existing ones.
;;
;;   Note that when rotating the underlining characters, if we come to the end
;;   of the hierarchy of characters, the variable rest-preferred-characters
;;   is consulted to propose a new underline char, and if continued, we cycle
;;   the underline characters all over again.  Set this variable to nil if 
;;   you want to limit the underlining character propositions to the existing
;;   underlines in the file.
;;
;; - prefix argument is used to alternate the sectioning style.
;;
;; Examples:
;;
;;   |Some Title       --->    |Some Title
;;   | 			       |----------
;;
;;   |Some Title       --->    |Some Title
;;   |----- 		       |----------
;;
;;   |                         |------------
;;   | Some Title      --->    | Some Title
;;   | 			       |------------
;;
;; In overline-and-underline style, a variable is available to select how much
;; space to leave before and after the title (it can be zero) when alternating
;; the style.  Note that if the title already has some whitespace in front of
;; it, we don't adjust it to the variable setting, we use the whitespace that is
;; already there for adjustment.

(defun rest-line-single-char-p (&optional accept-special)
  "Predicate return the unique char if the current line is
  composed only of a single repeated non-whitespace
  character. This returns the char even if there is whitespace at
  the beginning of the line.

  If ACCEPT-SPECIAL is specified we do not ignore special sequences
  which normally we would ignore when doing a search on many lines.
  For example, normally we have cases to ignore commonly occuring
  patterns, such as :: or ...;  with the flag do not ignore them."
  (save-excursion
    (back-to-indentation)
    (if (not (looking-at "\n"))
	(let ((c (thing-at-point 'char)))
	  (if (and (looking-at (format "[%s]+\\s-*$" c))
		   (or accept-special
		       (and
			;; common patterns
			(not (looking-at "::\\s-*$"))
			(not (looking-at "\\.\\.\\.\\s-*$"))
			;; discard one char line
			(not (looking-at ".\\s-*$")) 
			)))
	      (string-to-char c))
	  ))
    ))

(defun rest-find-last-section-char ()
  "Looks backward for the last section char found in the file."

  (let (c)
    (save-excursion 
      (while (and (not c) (not (bobp)))
	(forward-line -1)
	(setq c (rest-line-single-char-p))
	))
    c))

(defun rest-current-section-char (&optional point)
  "Gets the section char around the current point."
  (save-excursion
    (if point (goto-char point))
    (let ((offlist '(0 1 -2))
	  loff
	  rval
	  c)
      (while offlist
	(forward-line (car offlist))
	(setq c (rest-line-single-char-p 1))
	(if c
	    (progn (setq offlist nil
			 rval c))
	  (setq offlist (cdr offlist)))
	)
      rval
      )))

(defun rest-initial-sectioning-style (&optional point)
  "Looks around point and attempts to determine the sectioning
  style, between simple-underline and overline-and-underline.  If
  there aren't any existing over/underlines, return nil."
  (save-excursion
    (if point (goto-char point))
    (let (ou)
      (save-excursion
	(setq ou (mapcar
		  (lambda (x)
		    (forward-line x)
		    (rest-line-single-char-p))
		  '(-1 2))))
      (beginning-of-line)
      (cond
       ((equal ou '(nil nil)) nil)
       ((car ou) 'over-and-under) ;; we only need check the overline
       (t 'simple)
       )
      )))

(defun rest-all-section-chars (&optional ignore-lines)
  "Finds all the section chars in the entire file and orders them
  hierarchically, removing duplicates.  Basically, returns a list
  of the section underlining characters.

  Optional parameters IGNORE-AROUND can be a list of lines to
  ignore."

  (let (chars 
	c
	(curline 1))
    (save-excursion
      (beginning-of-buffer)
      (while (< (point) (buffer-end 1))
	(if (not (memq curline ignore-lines))
	    (progn
	      (setq c (rest-line-single-char-p))
	      (if c
		  (progn
		    (add-to-list 'chars c t)
		    ))) )
	(forward-line 1) (setq curline (+ curline 1))
	))
    chars))

(defun rest-suggest-new-char (allchars)
  "Given the last char that has been seen, suggest a new,
  different character, different from all that have been seen."
  (let ((potentials (copy-sequence rest-preferred-characters)))
    (dolist (x allchars)
      (setq potentials (delq x potentials))
      )
    (car potentials)
    ))

(defun rest-update-section (underlinechar style &optional indent)
  "Unconditionally updates the overline/underline of a section
  title using the given character CHAR, with STYLE 'simple or
  'over-and-under, in which case with title whitespace separation
  on each side with INDENT whitespaces.  If the style is 'simple,
  whitespace before the title is removed.

  If there are existing overline and/or underline, they are
  removed before adding the requested adornments."

  (interactive)
  (let (marker
	len
	ec
	(c ?-))

      (end-of-line)
      (setq marker (point-marker))

      ;; Fixup whitespace at the beginning and end of the line
      (if (or (null indent) (eq style 'simple))
	  (setq indent 0))
      (beginning-of-line)
      (delete-horizontal-space)
      (insert (make-string indent ? ))

      (end-of-line)
      (delete-horizontal-space)

      ;; Set the current column, we're at the end of the title line
      (setq len (+ (current-column) indent))

      ;; Remove previous line if it consists only of a single repeated character
      (save-excursion
	(forward-line -1)
	(and (rest-line-single-char-p 1)
	     (kill-line 1)))

      ;; Remove following line if it consists only of a single repeated character
      (save-excursion
	(forward-line +1)
	(and (rest-line-single-char-p 1)
	     (kill-line 1))
	;; Add a newline if we're at the end of the buffer, for the subsequence
	;; inserting of the underline
	(if (= (point) (buffer-end 1))
	    (newline 1)))

      ;; Insert overline
      (if (eq style 'over-and-under)
	  (save-excursion
	    (beginning-of-line)
	    (open-line 1)
	    (insert (make-string len underlinechar))))

      ;; Insert underline
      (forward-line +1)
      (open-line 1)
      (insert (make-string len underlinechar))

      (forward-line +1)
      (goto-char marker)
      ))

(defvar rest-preferred-characters '(?= ?- ?~ ?+ ?` ?# ?@)
  "Preferred ordering of underline characters.  This sequence is
  consulted to offer a new underline character when we rotate the 
  underlines at the end of the existing hierarchy of characters.")

(defvar rest-default-under-and-over-indent 1
  "Number of characters to indent the section title when toggling
  sectioning styles.  This is used when switching from a simple 
  section style to a over-and-under style.")

(defun rest-adjust-section-title ()
  "Adjust/rotate the section underlining for the section around
  point.

  This function is the main entry point of this module and is a
  bit of a swiss knife.  It is meant as the single function to
  invoke to adjust the underlines (and possibly overlines) of a
  section title in restructuredtext.  The next action it takes
  depends on context around the point, and it is meant to be
  invoked possibly more than once. Basically, this function deals
  with:

  - underlining a title if it does not have an underline;
  - adjusting the length of the underline characters to fit a
    modified title;
  - rotating the underlines/overlines in the set of already
    existing underline chars used in the file;
  - switching between simple underline and over-and-under style
    sectioning (or box style).

  Here are the gory details:

  - If the current line has no underline character around it,
    search backwards for a previously used underlining character,
    and underline the current line as a section title (also see
    prefix argument below).

    If no pre-existing underlining character is found in the on
    the line, we use the last seen underline char or consult the
    first element of rest-preferred-characters if this is the
    first title in the entire file.

  - If the current line does have an underline or overline, and
    if

    - the underline do not extend to exactly the end of the
      title line, this changes the length of the under(over)lines
      to fit exactly the section title;

    - the underline length is already adjusted to the end of the
      title line, we search the file for the underline chars, and
      we rotate the current title's underline character with that
      list (going down the hierarchy that is present in the
      file);

    If there is a prefix argument, switch the style between the
    initial sectioning style and the other sectioning style.  The
    two styles are overline-and-underline and simple-underline.
    
    If however, you are on a complete section title and you
    specify a negative argument, the effect of the prefix
    argument is to change the direction of rotation of the
    underline characters. Thus using a prefix argument and a
    negative prefix argument achieves a different result in the
    case of rotation.

    Note that the initial style of underlining (simple underline
    or box-style) depends on if there is whitespace at the start
    of the line.  If there are already underlines/overlines,
    those are used to select the style, otherwise if there is
    whitespace at the front of the title overline-and-underline
    style is chosen, and otherwise simple underline.

    Also, note that this should work on the section title line as
    well as on a complete or incomplete underline for a
    title (first thing we check for that case and move the cursor
    up a line if needed)."

  (interactive)

  (let* (
	 ;; check if we're on an underline under a title line, and move the
	 ;; cursor up if it is so.
	 (moved
	  (if (and (or (rest-line-single-char-p 1) 
		       (looking-at "^\\s-*$"))
		   (save-excursion
		     (forward-line -1)
		     (beginning-of-line)
		     (looking-at "^.+$")))
	      (progn (forward-line -1) t)
	    ))

	 ;; find current sectioning character
	 (curchar (rest-current-section-char))
	 ;; find current sectioning style
	 (init-style (rest-initial-sectioning-style))
	 ;; find current indentation of title line
	 (curindent (save-excursion
		      (back-to-indentation)
		      (current-column)))

	 ;; ending column
	(endcol (- (save-excursion
		     (end-of-line)
		     (current-column))
                   (save-excursion
		     (back-to-indentation)
                     (current-column))))
	 )

    ;; if there is no current style found...
    (if (eq init-style nil)
	;; select based on the whitespace at the beginning of the line
	(save-excursion
	  (beginning-of-line)
	  (setq init-style
		(if (looking-at "^\\s-+") 'over-and-under 'simple))))

    ;; if we're switching characters, we're going to simply change the
    ;; sectioning style.  this branch is also taken if there is no current
    ;; sectioning around the title.
    (if (or (and current-prefix-arg
		 (not (< (prefix-numeric-value current-prefix-arg) 0)))
	    (eq curchar nil))
	
	;; we're switching characters or there is currently no sectioning
	(progn
	  (setq curchar 
		(or curchar
		    (rest-find-last-section-char)
		    (car (rest-all-section-chars))
		    (car rest-preferred-characters)
		    ?=))

	  ;; if there is a current indent, reuse it, otherwise use default
	  (if (= curindent 0)
	      (setq curindent rest-default-under-and-over-indent))

	  (rest-update-section
	   curchar
	   (if (and current-prefix-arg
		    (not (< (prefix-numeric-value current-prefix-arg) 0)))
	       (if (eq init-style 'over-and-under) 'simple 'over-and-under)
	     init-style)
	   curindent)
	  )

      ;; else we're not switching characters, and there is some sectioning
      ;; already present, so check if the current sectioning is complete and
      ;; correct.
      (let ((exps (concat "^" 
			  (regexp-quote (make-string 
					 (+ endcol curindent) curchar)) 
			  "$")))
	(if (or
	     (not (save-excursion (forward-line +1)
				  (beginning-of-line)
				  (looking-at exps)))
	     (and (eq init-style 'over-and-under)
		  (not (save-excursion (forward-line -1)
				       (beginning-of-line)
				       (looking-at exps)))))

	    ;; the current sectioning needs to be fixed/updated!
	    (rest-update-section curchar init-style curindent)

	  ;; the current sectioning is complete, rotate characters
	  (let* ( (curline (+ (count-lines (point-min) (point))
			      (if (bolp) 1 0)))
		  (allchars (rest-all-section-chars 
			     (list (- curline 1) curline (+ curline 1))))

		  (rotchars 
		   (append allchars 
			   (filter 'identity
				   (list 
				    ;; suggest a new char
				    (rest-suggest-new-char allchars)
				    ;; rotate to first char
				    (car allchars)))))
		  (nextchar 
		   (or (cadr (memq curchar 
				   (if (< (prefix-numeric-value 
					   current-prefix-arg) 0)
				       (reverse rotchars) rotchars)))
		       (car allchars)) ) )


	    (if nextchar
		(rest-update-section nextchar init-style curindent))
	    )))
      )

    (if moved 
	(progn (forward-line 1) (end-of-line)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generic character repeater function.
;;
;; For sections, better to use the specialized function above, but this can
;; be useful for creating separators.

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section movement commands.
;;

;; Note: this is not quite correct, the definition is any non alpha-numeric
;; character.
(defun rest-title-char-p (c)
  "Returns true if the given character is a valid title char."
  (and (string-match "[-=`:\\.'\"~^_*+#<>!$%&(),/;?@\\\|]"
		     (char-to-string c)) t))

(defun rest-forward-section ()
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

(defun rest-backward-section ()
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


;;------------------------------------------------------------------------------
;; For backwards compatibility.  Remove at some point.
(defalias 'reST-title-char-p 'rest-title-char-p)
(defalias 'reST-forward-title 'rest-forward-section)
(defalias 'reST-backward-title 'rest-backward-section)


(provide 'restructuredtext)
