;; Copyright 2003 Stefan Merten
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

;;; rst-mode.el --- Mode for viewing and editing reStructuredText-documents.
;;
;; Add the following lines to your `.emacs' file:
;;
;; (autoload 'rst-mode "rst-mode" "mode for editing reStructuredText documents" t)
;; (setq auto-mode-alist
;;       (append '(("\\.rst$" . rst-mode)
;;                 ("\\.rest$" . rst-mode)) auto-mode-alist))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defvar rst-mode-lazy t
  "If non-nil `rst-mode' font-locks comment, literal blocks, and sections
correctly. Because this is really slow it switches on `lazy-lock-mode'
automatically. You may increase `lazy-lock-defer-time' for reasonable results.

If nil comments and literal blocks are font-locked only on the line they start.

The value of this variable is used when `rst-mode' is loaded.")

;; Faces for displaying items on several levels
(defconst rst-level-face-max 6
  "Maximum depth of level faces defined")
(defconst rst-level-face-base-color "grey"
  "The base color to be used for creating level faces")
(defconst rst-level-face-base-light 85
  "The lightness factor for the base color")
(defconst rst-level-face-format-light "%2d"
  "The format for the lightness factor for the base color")
(defconst rst-level-face-step-light -7
  "The step width to use for next color")

;; Define the faces
(let ((i 1))
  (while (<= i rst-level-face-max)
    (let ((sym (intern (format "rst-level-%d-face" i)))
	  (doc (format "Face for showing items at level %d" i))
	  (col (format (concat "%s" rst-level-face-format-light)
		       rst-level-face-base-color
		       (+ (* (1- i) rst-level-face-step-light)
			  rst-level-face-base-light))))
      (make-empty-face sym)
      (set-face-doc-string sym doc)
      (set-face-background sym col)
      (set sym sym)
    (setq i (1+ i)))))

(defvar rst-adornment-faces-alist
  '((1 . rst-level-1-face)
    (2 . rst-level-2-face)
    (3 . rst-level-3-face)
    (4 . rst-level-4-face)
    (5 . rst-level-5-face)
    (6 . rst-level-6-face)
    (t . font-lock-keyword-face)
    (nil . font-lock-keyword-face))
  "An alist mapping adornment types to faces. Key is a number (for faces for
section texts on that level), t (for a face for transitions) or nil (default
face used for section adornment).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rst-mode-map nil
  "Keymap for rst mode.")

(defvar rst-mode-syntax-table nil
  "Syntax table used while in rst mode.")

(defun rst-mode ()
  "Major mode for editing reStructuredText documents.

You may customize `rst-mode-lazy' to switch font-locking of blocks.

\\{rst-mode-map}
Turning on rst mode calls the value of the variable `rst-mode-hook',
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map rst-mode-map)
  (setq mode-name "reST")
  (setq major-mode 'rst-mode)
;  (setq local-abbrev-table rst-mode-abbrev-table)
  (set-syntax-table rst-mode-syntax-table)
  (set (make-local-variable 'comment-start) ".. ")
  (set (make-local-variable 'indent-line-function) 'indent-relative-maybe)
  (set (make-local-variable 'font-lock-multiline) t)
  (make-local-variable 'rst-font-lock-adornment-section-alist)
  (run-hooks 'rst-mode-hook))

(unless rst-mode-syntax-table
  (setq rst-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\" "." rst-mode-syntax-table)
  (modify-syntax-entry ?$ "." rst-mode-syntax-table)
  (modify-syntax-entry ?% "." rst-mode-syntax-table)
  (modify-syntax-entry ?& "." rst-mode-syntax-table)
  (modify-syntax-entry ?* "." rst-mode-syntax-table)
  (modify-syntax-entry ?+ "." rst-mode-syntax-table)
  (modify-syntax-entry ?. "_" rst-mode-syntax-table)
  (modify-syntax-entry ?/ "." rst-mode-syntax-table)
  (modify-syntax-entry ?< "." rst-mode-syntax-table)
  (modify-syntax-entry ?= "." rst-mode-syntax-table)
  (modify-syntax-entry ?> "." rst-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" rst-mode-syntax-table)
  (modify-syntax-entry ?| "." rst-mode-syntax-table)
  )

;(defvar rst-mode-abbrev-table nil
;  "Abbrev table used while in rst mode.")
;(define-abbrev-table 'rst-mode-abbrev-table ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rst-font-lock-keywords
    ;; The rst-links in the following comments all relate to sections in
    ;; http://docutils.sourceforge.net/spec/rst/reStructuredText.html
  (let* ( ;; This gets big - so let's define some abbreviations
	 ;; all syntax marking up a special block
	 (face-block '(quote font-lock-keyword-face))
	 ;; field names and interpreted text
	 (face-external '(quote font-lock-type-face))
	 ;; all other defining constructs
	 (face-definition '(quote font-lock-function-name-face))
	 ;; directives and roles
	 (face-directive '(quote font-lock-builtin-face))
	 (face-comment '(quote font-lock-comment-face))
	 (face-emphasis1 '(quote italic))
	 (face-emphasis2 '(quote bold))
	 (face-literal '(quote font-lock-string-face))
	 ;; references to a definition
	 (face-reference '(quote font-lock-variable-name-face))

	 ;; horizontal white space
	 (re-hws "[\t ]")
	 ;; beginning of line with possible indentation
	 (re-bol (concat "^" re-hws "*"))
	 ;; Separates block lead-ins from their content
	 (re-blksep1 (concat "\\(" re-hws "+\\|$\\)"))
	 ;; explicit markup tag
	 (re-emt "\\.\\.")
	 ;; explicit markup start
	 (re-ems (concat re-emt re-hws "+"))
	 ;; inline markup prefix
	 (re-imp1 (concat "\\(^\\|" re-hws "\\|[-'\"([{</:]\\)"))
	 ;; inline markup suffix
	 (re-ims1 (concat "\\(" re-hws "\\|[]-'\")}>/:.,;!?\\]\\|$\\)"))
	 ;; symbol character
	 (re-sym1 "\\(\\sw\\|\\s_\\)")
	 ;; inline markup content begin
	 (re-imbeg2 "\\(\\S \\|\\S \\([^")

	 ;; There seems to be a bug leading to error "Stack overflow in regexp
	 ;; matcher" when "|" or "\\*" are the characters searched for
	 (re-imendbeg
	  (if (< emacs-major-version 21)
	      "]"
	    "\\]\\|\\\\."))
	 ;; inline markup content end
	 (re-imend (concat re-imendbeg "\\)*[^\t \\\\]\\)"))
	 ;; inline markup content without asterisk
	 (re-ima2 (concat re-imbeg2 "*" re-imend))
	 ;; inline markup content without backquote
	 (re-imb2 (concat re-imbeg2 "`" re-imend))
	 ;; inline markup content without vertical bar
	 (re-imv2 (concat re-imbeg2 "|" re-imend))
	 ;; Supported URI schemes
	 (re-uris1 "\\(acap\\|cid\\|data\\|dav\\|fax\\|file\\|ftp\\|gopher\\|http\\|https\\|imap\\|ldap\\|mailto\\|mid\\|modem\\|news\\|nfs\\|nntp\\|pop\\|prospero\\|rtsp\\|service\\|sip\\|tel\\|telnet\\|tip\\|urn\\|vemmi\\|wais\\)")
	 ;; Line starting with adornment and optional whitespace; complete
	 ;; adornment is in (match-string 1); there must be at least 3
	 ;; characters because otherwise explicit markup start would be
	 ;; recognized
	 (re-ado2 (concat "^\\(\\(["
			  (if (< emacs-major-version 21)
			      "^a-zA-Z0-9 \t\x00-\x1F"
			    "^[:word:][:space:][:cntrl:]")
			  "]\\)\\2\\2+\\)" re-hws "*$"))
	 )
    (list
     ;; FIXME: Block markup is not recognized in blocks after explicit markup
     ;; start

     ;; Simple `Body Elements`_
     ;; `Bullet Lists`_
     (list
      (concat re-bol "\\([-*+]" re-blksep1 "\\)")
      1 face-block)
     ;; `Enumerated Lists`_
     (list
      (concat re-bol "\\((?\\([0-9]+\\|[A-Za-z]\\|[IVXLCMivxlcm]+\\)[.)]" re-blksep1 "\\)")
      1 face-block)
     ;; `Definition Lists`_ FIXME: missing
     ;; `Field Lists`_
     (list
      (concat re-bol "\\(:[^:]+:\\)" re-blksep1)
      1 face-external)
     ;; `Option Lists`_
     (list
      (concat re-bol "\\(\\(\\(\\([-+/]\\|--\\)\\sw\\(-\\|\\sw\\)*\\([ =]\\S +\\)?\\)\\(,[\t ]\\)?\\)+\\)\\($\\|[\t ]\\{2\\}\\)")
      1 face-block)

     ;; `Tables`_ FIXME: missing

     ;; All the `Explicit Markup Blocks`_
     ;; `Footnotes`_ / `Citations`_
     (list
      (concat re-bol "\\(" re-ems "\\[[^[]+\\]\\)" re-blksep1)
      1 face-definition)
     ;; `Directives`_ / `Substitution Definitions`_
     (list
      (concat re-bol "\\(" re-ems "\\)\\(\\(|[^|]+|[\t ]+\\)?\\)\\(" re-sym1 "+::\\)" re-blksep1)
      (list 1 face-directive)
      (list 2 face-definition)
      (list 4 face-directive))
     ;; `Hyperlink Targets`_
     (list
      (concat re-bol "\\(" re-ems "_\\([^:\\`]\\|\\\\.\\|`[^`]+`\\)+:\\)" re-blksep1)
      1 face-definition)
     (list
      (concat re-bol "\\(__\\)" re-blksep1)
      1 face-definition)

     ;; All `Inline Markup`_
     ;; FIXME: Condition 5 preventing fontification of e.g. "*" not implemented
     ;; `Strong Emphasis`_
     (list
      (concat re-imp1 "\\(\\*\\*" re-ima2 "\\*\\*\\)" re-ims1)
      2 face-emphasis2)
     ;; `Emphasis`_
     (list
      (concat re-imp1 "\\(\\*" re-ima2 "\\*\\)" re-ims1)
      2 face-emphasis1)
     ;; `Inline Literals`_
     (list
      (concat re-imp1 "\\(``" re-imb2 "``\\)" re-ims1)
      2 face-literal)
     ;; `Inline Internal Targets`_
     (list
      (concat re-imp1 "\\(_`" re-imb2 "`\\)" re-ims1)
      2 face-definition)
     ;; `Hyperlink References`_
     ;; FIXME: `Embedded URIs`_ not considered
     (list
      (concat re-imp1 "\\(\\(`" re-imb2 "`\\|\\sw+\\)__?\\)" re-ims1)
      2 face-reference)
     ;; `Interpreted Text`_
     (list
      (concat re-imp1 "\\(\\(:" re-sym1 "+:\\)?\\)\\(`" re-imb2 "`\\)\\(\\(:" re-sym1 "+:\\)?\\)" re-ims1)
      (list 2 face-directive)
      (list 5 face-external)
      (list 8 face-directive))
     ;; `Footnote References`_ / `Citation References`_
     (list
      (concat re-imp1 "\\(\\[[^]]+\\]_\\)" re-ims1)
      2 face-reference)
     ;; `Substitution References`_
     (list
      (concat re-imp1 "\\(|" re-imv2 "|\\)" re-ims1)
      2 face-reference)
     ;; `Standalone Hyperlinks`_
     (list
      ;; FIXME: This takes it easy by using a whitespace as delimiter
      (concat re-imp1 "\\(" re-uris1 ":\\S +\\)" re-ims1)
      2 face-definition)
     (list
      (concat re-imp1 "\\(" re-sym1 "+@" re-sym1 "+\\)" re-ims1)
      2 face-definition)

     ;; Do all block fontification as late as possible so 'append works

     ;; Sections_ / Transitions_
     (append
      (list
       re-ado2)
      (if (not rst-mode-lazy)
	  (list 1 face-block)
	(list
	 (list 'rst-font-lock-handle-adornment
	       '(progn
		  (setq rst-font-lock-adornment-point (match-end 1))
		  (point-max))
	       nil
	       (list 1 '(cdr (assoc nil rst-adornment-faces-alist))
		     'append t)
	       (list 2 '(cdr (assoc rst-font-lock-section rst-adornment-faces-alist))
		     'append t)
	       (list 3 '(cdr (assoc nil rst-adornment-faces-alist))
		     'append t)))))

     ;; `Comments`_
     (append
      (list
       (concat re-bol "\\(" re-ems "\\)\[^[|_]\\([^:]\\|:\\([^:]\\|$\\)\\)*$")
       (list 1 face-comment))
      (if rst-mode-lazy
	  (list
	   (list 'rst-font-lock-find-unindented-line
		 '(progn
		    (setq rst-font-lock-indentation-point (match-end 1))
		    (point-max))
		 nil
		 (list 0 face-comment 'append)))))
     (append
      (list
       (concat re-bol "\\(" re-emt "\\)\\(\\s *\\)$")
       (list 1 face-comment)
       (list 2 face-comment))
      (if rst-mode-lazy
	  (list
	   (list 'rst-font-lock-find-unindented-line
		 '(progn
		    (setq rst-font-lock-indentation-point 'next)
		    (point-max))
		 nil
		 (list 0 face-comment 'append)))))

     ;; `Literal Blocks`_
     (append
      (list
       (concat re-bol "\\(\\([^.\n]\\|\\.[^.\n]\\).*\\)?\\(::\\)$")
       (list 3 face-block))
      (if rst-mode-lazy
	  (list
	   (list 'rst-font-lock-find-unindented-line
		 '(progn
		    (setq rst-font-lock-indentation-point t)
		    (point-max))
		 nil
		 (list 0 face-literal 'append)))))
     ))
  "Keywords to highlight in rst mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indented blocks

(defun rst-move-forward-unindented-line (&optional limit column)
  "Find the next non-empty line which is not indented at least to COLUMN.
COLUMN defaults to the column of the point. Moves point to first character of
this line and returns that position. Trailing empty lines do not count as being
part of the indented block. If there is no such line before LIMIT returns nil
and point is not moved. LIMIT defaults to (point-max)."
  (interactive)
  (let ((clm (or column (current-column)))
	(start (point))
	fnd beg cand)
    (if (not limit)
	(setq limit (point-max)))
    (save-match-data
      (while (and (not fnd) (< (point) limit))
	(forward-line 1)
	(when (< (point) limit)
	  (setq beg (point))
	  (if (looking-at "\\s *$")
	      (setq cand (or cand beg)) ; An empty line is a candidate
	    (move-to-column clm)
	    (if (string-match
		 "^\\s *$" (buffer-substring-no-properties beg (point)))
		(setq cand nil) ; An indented line resets a candidate
	      (setq fnd (or cand beg)))))))
    (goto-char (or fnd start))
    fnd))

; Stores the point where the current indentation ends if a number. If `next'
; indicates `rst-font-lock-find-unindented-line' shall take the indentation
; from the next line if this is not empty. If non-nil indicates
; `rst-font-lock-find-unindented-line' shall take the indentation from the next
; non-empty line. Also used as a trigger for
; `rst-font-lock-find-unindented-line'.
(defvar rst-font-lock-indentation-point nil)

(defun rst-font-lock-find-unindented-line (limit)
  (let* ((ind-pnt rst-font-lock-indentation-point)
	 (beg-pnt ind-pnt))
    ;; May run only once - enforce this
    (setq rst-font-lock-indentation-point nil)
    (when (and ind-pnt (not (numberp ind-pnt)))
      ;; Find indentation point in next line if any
      (setq ind-pnt
	    (save-excursion
	      (save-match-data
		(if (eq ind-pnt 'next)
		    (when (and (zerop (forward-line 1)) (< (point) limit))
		      (setq beg-pnt (point))
		      (when (not (looking-at "\\s *$"))
			(looking-at "\\s *")
			(match-end 0)))
		  (while (and (zerop (forward-line 1)) (< (point) limit)
			      (looking-at "\\s *$")))
		  (when (< (point) limit)
		    (setq beg-pnt (point))
		    (looking-at "\\s *")
		    (match-end 0)))))))
    (when ind-pnt
      (goto-char ind-pnt)
      ;; Always succeeds because the limit set by PRE-MATCH-FORM is the
      ;; ultimate point to find
      (goto-char (or (rst-move-forward-unindented-line limit) limit))
      (set-match-data (list beg-pnt (point)))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adornments

; Stores the point where the current adornment ends. Also used as a trigger for
; `rst-font-lock-handle-adornment'.
(defvar rst-font-lock-adornment-point nil)

; Here `rst-font-lock-handle-adornment' stores the section level of the current
; adornment or t for a transition.
(defvar rst-font-lock-section nil)

; Associates adornments with section levels as encountered while font-locking.
; The key is a two character string. The first character is the adornment
; character. The second character distinguishes underline sections (`u'),
; overline/underline sections (`o'). The value is the section level.
;
; This is made buffer local on start and adornments found during font lock are
; entered.
(defvar rst-font-lock-adornment-section-alist nil)

; Handles adornments for sections and transitions. Returns three match groups.
; First and last match group matched pure adornment while second group matched
; section text.
(defun rst-font-lock-handle-adornment (limit)
  (let* ((ado-pnt rst-font-lock-adornment-point))
    ;; May run only once - enforce this
    (setq rst-font-lock-adornment-point nil)
    (when ado-pnt
      (let ((ado-ch (aref (match-string-no-properties 1) 0))
	    (ado-re (regexp-quote (match-string-no-properties 1)))
	    (nxt-emp
	     (save-excursion
	       (save-match-data
		 (or (not (zerop (forward-line 1)))
		     (looking-at "\\s *$")))))
	    (prv-emp
	     (save-excursion
	       (save-match-data
		 (or (not (zerop (forward-line -1)))
		     (looking-at "\\s *$")))))
	    (beg-pnt
	     (save-excursion
	       (forward-line 0)
	       (point)))
	    (end-pnt ado-pnt)
	    beg-ovr end-ovr beg-und end-und ado-key)
	(cond
	 ((and nxt-emp prv-emp)
	  ;; A transition
	  (setq rst-font-lock-section t))
	 (prv-emp
	  ;; An overline
	  (setq ado-key (concat (list ado-ch) "o"))
	  (setq beg-ovr beg-pnt)
	  (setq end-ovr end-pnt)
	  (save-excursion
	    (save-match-data
	      (forward-line 1)
	      (setq beg-pnt (point))
	      (setq end-pnt nil)
	      (while (and (< (point) limit) (not end-pnt))
		(if (looking-at "\\s *$")
		    ;; No underline found
		    (setq end-pnt (1- (point)))
		  (when (looking-at (concat "\\(" ado-re "\\)\\s *$"))
		    (setq end-und (match-end 1))
		    (forward-line 0)
		    (setq beg-und (point))
		    (setq end-pnt (1- beg-und))))
		(forward-line 1)))))
	 (t
	  ;; An underline
	  (setq ado-key (concat (list ado-ch) "u"))
	  (setq beg-und beg-pnt)
	  (setq end-und end-pnt)
	  (setq end-pnt (1- beg-und))
	  (setq beg-pnt
		(save-excursion
		  (save-match-data
		    (re-search-backward "^\\s *$" 1 'move)
		    (forward-line 1)
		    (point))))))
	(when ado-key
	  (unless (assoc ado-key rst-font-lock-adornment-section-alist)
	    (let ((new 1))
	      (while (rassoc new rst-font-lock-adornment-section-alist)
		(setq new (1+ new)))
	      (setq rst-font-lock-adornment-section-alist
		    (append rst-font-lock-adornment-section-alist
			    (list (cons ado-key new))))))
	  (setq rst-font-lock-section
		(cdr (assoc ado-key rst-font-lock-adornment-section-alist))))
	(goto-char (or end-und end-pnt end-ovr))
	(set-match-data (list (or beg-ovr beg-pnt beg-und)
			      (or end-und end-pnt end-und)
			      beg-ovr end-ovr beg-pnt end-pnt beg-und end-und))
	t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; rst-mode has its own mind about font-lock-support-mode
(if (boundp 'font-lock-support-mode)
    (cond
     ((and (not rst-mode-lazy) (not font-lock-support-mode)))
     ;; No support mode set and none wanted - leave it alone
     ((or (not font-lock-support-mode) ;; No support mode set (but wanted)
	  (symbolp font-lock-support-mode)) ;; or a fixed mode for all
      (setq font-lock-support-mode
	    (list (cons 'rst-mode (and rst-mode-lazy 'lazy-lock-mode))
		  (cons t font-lock-support-mode))))
     ((and (listp font-lock-support-mode)
	   (not (assoc 'rst-mode font-lock-support-mode)))
      ;; A list of modes missing rst-mode
      (setq font-lock-support-mode
	    (append '((cons 'rst-mode (and rst-mode-lazy 'lazy-lock-mode)))
		    font-lock-support-mode)))))

(if (string-match "XEmacs\\|Lucid" emacs-version)
    (put 'rst-mode 'font-lock-defaults '(rst-font-lock-keywords t))
  (setq font-lock-defaults-alist
	(append '((rst-mode rst-font-lock-keywords t))
		font-lock-defaults-alist)))

;;; rst-mode.el ends here
