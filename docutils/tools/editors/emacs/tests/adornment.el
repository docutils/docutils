;; Tests for various functions handling adornments

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-line-homogeneous-p ()
  "Tests for `rst-line-homogeneous-p'."
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "Blablabla bla\^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "-----------\^@"
	   nil
	   ?-))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "   -----------\^@"
	   nil
	   ?-))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "aaaa\^@aaa"
	   nil
	   ?a))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "uuuuuuuuuuuuuuuuu\^@"
	   nil
	   ?u))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "--=---------\^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   " uuuuuuuuuuuuuuuuu\^@"
	   nil
	   ?u))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   " uuuuuuuuuuuuuuuuu \^@"
	   nil
	   ?u))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "-------\^@----"
	   nil
	   ?-))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-p)
	   "-\^@"
	   nil
	   nil))
  )

(ert-deftest rst-line-homogeneous-nodent-p ()
  "Tests for `rst-line-homogeneous-nodent-p'."
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "Blablabla bla\^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "-----------\^@"
	   nil
	   ?-))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "   -----------\^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "aaaa\^@aaa"
	   nil
	   ?a))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "uuuuuuuuuuuuuuuuu\^@"
	   nil
	   ?u))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "--=---------\^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   " uuuuuuuuuuuuuuuuu\^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   " uuuuuuuuuuuuuuuuu \^@"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "-------\^@----"
	   nil
	   ?-))
  (should (equal-buffer-return
	   '(rst-line-homogeneous-nodent-p)
	   "-\^@"
	   nil
	   nil))
  )

(ert-deftest rst-normalize-cursor-position ()
  "Tests for `rst-normalize-cursor-position'."
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "

Du bon vin tous les jours.
\^@
"
	   "

\^@Du bon vin tous les jours.

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "
\^@
Du bon vin tous les jours.

"
	   "

\^@Du bon vin tous les jours.

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "

Du bon vin tous les jours.
------\^@-----
"
	   "

\^@Du bon vin tous les jours.
-----------
"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "
------\^@-----
Du bon vin tous les jours.

"
	   "
-----------
\^@Du bon vin tous les jours.

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "
\^@-----------
Du bon vin tous les jours.
-----------

"
	   "
-----------
\^@Du bon vin tous les jours.
-----------

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "
Du bon vin tous les jours.
\^@-----------
Du bon vin tous les jours.
-----------

"
	   "
\^@Du bon vin tous les jours.
-----------
Du bon vin tous les jours.
-----------

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "

\^@-----------

"
	   "

\^@-----------

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "
Line 1
\^@
Line 2

"
	   "
\^@Line 1

Line 2

"
	   ))
  (should (equal-buffer
	   '(rst-normalize-cursor-position)
	   "
=====================================
   Project Idea: Panorama Stitcher
====================================

:Author: Martin Blais <blais@furius.ca>
\^@
Another Title
=============
"
	   "
=====================================
   Project Idea: Panorama Stitcher
====================================

\^@:Author: Martin Blais <blais@furius.ca>

Another Title
=============
"
	   ))
  )

(ert-deftest rst-get-adornment ()
  "Tests for `rst-get-adornment'."
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@Du bon vin tous les jours

"
	   nil
	   '(nil nil 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@
Du bon vin tous les jours

"
	   nil
	   '(nil nil 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@  Du bon vin tous les jours

"
	   nil
	   '(nil nil 2)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@Du bon vin tous les jours
=========================

"
	   nil
	   '(?= simple 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@Du bon vin tous les jours
====================

"
	   nil
	   '(?= simple 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@     Du bon vin tous les jours
====================

"
	   nil
	   '(?= simple 5)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@Du bon vin tous les jours
-
"
	   nil
	   '(nil nil 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

\^@Du bon vin tous les jours
--
"
	   nil
	   '(?- simple 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
~~~~~~~~~~~~~~~~~~~~~~~~~
\^@Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '(?~ over-and-under 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "~~~~~~~~~~~~~~~~~~~~~~~~~
\^@Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '(?~ over-and-under 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
~~~~~~~~~~~~~~~~~~~~~~~~~
\^@   Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '(?~ over-and-under 3)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\^@Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '(?~ over-and-under 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
---------------------------
\^@Du bon vin tous les jours
~~~~~~~~~~~~~~~~~~~~~~~~~~~

"
	   nil
	   '(?~ over-and-under 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

Du bon vin to\^@us les jours
=========================

"
	   nil
	   '(?= simple 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
\^@
=========================
Du bon vin tous les jours
=========================
"
	   nil
	   '(nil nil 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
Du bon vin\^@

"
	   nil
	   '(nil nil 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
Du bon vin\^@
----------

"
	   nil
	   '(45 simple 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
----------
Du bon vin\^@
----------

"
	   nil
	   '(45 over-and-under 0)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "
=========================
Du bon vin tous les jours
=========================
--------------
  Du bon vin\^@
--------------

"
	   nil
	   '(45 over-and-under 2)))
  (should (equal-buffer-return
	   '(rst-get-adornment)
	   "

  Du bon vin tous les jours\^@
  =========================

"
	   nil
	   '(nil nil 2)))
  )

(setq text-1
"===============================
   Project Idea: My Document
===============================

:Author: Martin Blais

Introduction
============

This is the introduction.

Notes
-----

Some notes.

Main Points
===========

Yep.

Super Point
-----------

~~~~~~~~~~~
\^@ Sub Point
~~~~~~~~~~~

Isn't this fabulous?

Conclusion
==========

That's it, really.

")

(setq text-2
"

Previous
--------

Current\^@
~~~~~~~

Next
++++

")

(setq text-3
"

Previous
--------

Current\^@
~~~~~~~

  Next
  ++++

")

(ert-deftest rst-find-all-adornments ()
  "Tests for `rst-find-all-adornments'."
  (should (equal-buffer-return
	   '(rst-find-all-adornments)
	   text-1
	   nil
	   '((2 61 over-and-under 3)
	     (7 61 simple 0)
	     (12 45 simple 0)
	     (17 61 simple 0)
	     (22 45 simple 0)
	     (26 126 over-and-under 1)
	     (31 61 simple 0))
	   ))
  (should (equal-buffer-return
	   '(rst-find-all-adornments)
	   text-2
	   nil
	   '((3 45 simple 0)
	     (6 126 simple 0)
	     (9 43 simple 0))
	   ))
  (should (equal-buffer-return
	   '(rst-find-all-adornments)
	   text-3
	   nil
	   '((3 45 simple 0)
	     (6 126 simple 0))
	   ))
  )

(ert-deftest rst-get-hierarchy ()
  "Tests for `rst-get-hierarchy'."
  (should (equal-buffer-return
	   '(rst-get-hierarchy)
	   text-1
	   nil
	   '((61 over-and-under 3)
	     (61 simple 0)
	     (45 simple 0)
	     (126 over-and-under 1))
	   ))
  )

(ert-deftest rst-get-hierarchy-ignore ()
  "Tests for `rst-get-hierarchy' with ignoring a line."
  (should (equal-buffer-return
	   '(rst-get-hierarchy nil 26)
	   text-1
	   nil
	   '((61 over-and-under 3)
	     (61 simple 0)
	     (45 simple 0))
	   ))
  )

(ert-deftest rst-adornment-complete-p ()
  "Tests for `rst-adornment-complete-p'."
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "

\^@Vaudou

"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
======
"
	   nil
	   t))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
======
"
	   nil
	   t))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 2))
	   "
==========
\^@  Vaudou
==========
"
	   nil
	   t))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
=====
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
=======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= simple 0))
	   "
\^@Vaudou
===-==
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
=====
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
=====
\^@Vaudou
======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
===-==
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
===-==
\^@Vaudou
======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou

"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
======
\^@Vaudou
------
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou
=========
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
=========
  \^@Vaudou
==========
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou
===-======
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
===-======
  \^@Vaudou
==========
"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou

"
	   nil
	   nil))
  (should (equal-buffer-return
	   '(rst-adornment-complete-p (?= over-and-under 0))
	   "
==========
  \^@Vaudou
----------
"
	   nil
	   nil))
  )

(ert-deftest rst-get-adornments-around ()
  "Tests for `rst-get-adornments-around'."
  (should (equal-buffer-return
	   '(rst-get-adornments-around)
	   "

Previous
--------

\^@Current

Next
++++

"
	   nil
	   '((?- simple 0) (?+ simple 0))))
  (should (equal-buffer-return
	   '(rst-get-adornments-around)
	   "

Previous
--------

Current\^@
~~~~~~~

Next
++++

"
	   nil
	   '((?- simple 0) (?+ simple 0))))
  )
