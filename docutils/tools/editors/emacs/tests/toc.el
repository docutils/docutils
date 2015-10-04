;; Tests for operations on toc

(add-to-list 'load-path ".")
(load "init" nil t)
(init-rst-ert t)

(ert-deftest toc-asserts ()
  "Check some assertions."
  (should (equal ert-Buf-point-char "\^@"))
  (should (equal ert-Buf-mark-char "\^?"))
  )

(ert-deftest rst-toc-insert ()
  "Tests `rst-toc-insert'."
  (let ((title "=====
Title
=====

")
	(headers "Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========"))
    ;; Set customizable variables to defaults
    (let ((rst-toc-indent 2)
	  (rst-toc-insert-style 'fixed)
	  (rst-toc-insert-number-separator "  ")
	  (rst-toc-insert-max-level nil))
      ;; Can't identify a title so do nothing - that's actually a (MIS-)FEATURE
      (should (ert-equal-buffer
	       (rst-toc-insert)
	       (concat "\^@" headers)
	       t))
      ;; Won't work on a section title
      (should (ert-equal-buffer
	       (rst-toc-insert)
	       (concat title "\^@" headers)
	       t))
      ;; No indentation
      (should (ert-equal-buffer
	       (rst-toc-insert)
	       (concat title "\^@\n\n" headers)
	       (concat title "1  Header A
2  Header B
  2.1  Subheader B.a
    2.1.1  SubSubheader B.a.1
  2.2  Subheader B.b
3  Header C\^@

" headers)))
      ;; Indentation
      (should (ert-equal-buffer
	       (rst-toc-insert)
	       (concat title "  \^@\n\n" headers)
	       (concat title "  1  Header A
  2  Header B
    2.1  Subheader B.a
      2.1.1  SubSubheader B.a.1
    2.2  Subheader B.b
  3  Header C\^@

" headers)))
      ;; Only first level
      (should (ert-equal-buffer
	       (rst-toc-insert 1)
	       (concat title "  \^@\n\n" headers)
	       (concat title "  1  Header A
  2  Header B
  3  Header C\^@

" headers)))
      ;; Prefix and indentation
      (should (ert-equal-buffer
	       (rst-toc-insert)
	       (concat title "..  \^@\n\n" headers)
	       (concat title "..  1  Header A
    2  Header B
      2.1  Subheader B.a
        2.1.1  SubSubheader B.a.1
      2.2  Subheader B.b
    3  Header C\^@

" headers)))
      )
    ))

(ert-deftest rst-toc-update ()
  "Tests `rst-toc-update'."
  (let ((title "=====
Title
=====

")
	(headers "Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========")
	(contents ".. contents:: Inhalt\n")
	(fields "   :bla: blub\n   :local:\n")
	(old "..  1  Header A
    2  Header B
    3  Header C")
	(new "..
    1  Header A
    2  Header B
      2.1  Subheader B.a
        2.1.1  SubSubheader B.a.1
      2.2  Subheader B.b
    3  Header C")
	)
    ;; Set customizable variables to defaults
    (let ((rst-toc-indent 2)
	  (rst-toc-insert-style 'fixed)
	  (rst-toc-insert-number-separator "  ")
	  (rst-toc-insert-max-level nil))
      (should (ert-equal-buffer
	       (rst-toc-update)
	       (concat title contents fields old "\n\n" headers "\^@")
	       (concat title contents fields new "\n\n" headers "\^@")))
      (should (ert-equal-buffer
	       (rst-toc-update)
	       (concat title contents old "\n\n" headers "\^@")
	       (concat title contents new "\n\n" headers "\^@")))
      )
    ))

(defun toc ()
  "Call `rst-toc' and copy special buffer to target buffer."
  (let ((wincfg (current-window-configuration))
	txt pt mrk)
    (if (get-buffer rst-toc-buffer-name)
	(kill-buffer rst-toc-buffer-name))
    (rst-toc)
    (with-current-buffer rst-toc-buffer-name
      (setq txt (buffer-substring-no-properties (point-min) (point-max)))
      (setq pt (point))
      (setq mrk (mark t)))
    (set-window-configuration wincfg)
    (kill-buffer rst-toc-buffer-name)
    (delete-region (point-min) (point-max))
    (insert txt)
    (set-mark mrk)
    (goto-char pt)))

(ert-deftest rst-toc ()
  "Tests `rst-toc'."
  ;; Set customizable variables to defaults
  (let ((rst-toc-indent 2))
    (should (ert-equal-buffer
	     (toc)
	     "=====
Title
=====

Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========
\^@"
	     "Table of Contents: 
Title
  Header A
  Header B
    Subheader B.a
      SubSubheader B.a.1
    Subheader B.b
\^@  Header C
"
	     ))
    (should (ert-equal-buffer
	     (toc)
	     "=====
Title
=====

Header A
========

Header B
========

Subh\^@eader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========
"
	     "Table of Contents: 
Title
  Header A
  Header B
\^@    Subheader B.a
      SubSubheader B.a.1
    Subheader B.b
  Header C
"
	     ))
    (should (ert-equal-buffer
	     (toc)
	     "\^@

=====
Title
=====

Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~

Subheader B.b
-------------

Header C
========
"
	     "\^@Table of Contents: 
Title
  Header A
  Header B
    Subheader B.a
      SubSubheader B.a.1
    Subheader B.b
  Header C
"
	     ))
    (should (ert-equal-buffer
	     (toc)
	     "=====
Title
=====

Header A
========

Header B
========

Subheader B.a
-------------

SubSubheader B.a.1
~~~~~~~~~~~~~~~~~~
\^@
Subheader B.b
-------------

Header C
========
"
	     "Table of Contents: 
Title
  Header A
  Header B
    Subheader B.a
\^@      SubSubheader B.a.1
    Subheader B.b
  Header C
"
	     ))
    ))

;; FIXME: More functions to test:
;; * rst-toc-mode-goto-section
