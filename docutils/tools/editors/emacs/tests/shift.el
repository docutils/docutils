;; Tests for various functions around shifting text

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(defun compute-tabs ()
  "Wrapper to call `rst-compute-tabs' creating a readable result."
  (let ((tabs (mapcar 'car (rst-compute-tabs (point)))))
    (delete-region (point-min) (point-max))
    (dolist (tab tabs)
      (insert "\n" (make-string tab ? ) "@"))))

(ert-deftest rst-compute-tabs ()
  "Tests for `rst-compute-tabs'."
  (should (equal-buffer
	   '(compute-tabs)
	   "\^@"
	   ""
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
\^@"
	   "
@
  @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
  * b
\^@"
	   "
@
  @
    @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
  * b
    XV. c
\^@"
	   "
@
  @
    @
        @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
    XV. c
\^@"
	   "
@
  @
    @
        @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* 
    XV. c
\^@"
	   "
@
    @
        @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
  * b
    XV. c
  * d
\^@"
	   "
@
  @
    @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
  * b
    XV. c
  * d
* e
\^@"
	   "
@
  @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
  * a
    * b
      XV. c
\^@"
           "
  @
    @
      @
          @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "
* a
  *\tb
\^@"
	   "
@
  @
        @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "* a
\^@"
	   "
@
  @"
	   ))
  (should (equal-buffer
	   '(compute-tabs)
	   "  * a
    * b
      XV. c
\^@"
           "
  @
    @
      @
          @"
	   ))
  )

(ert-deftest rst-shift-region-right ()
  "Tests for `rst-shift-region' to the right."
  (let ((rst-indent-width 2)) ; Set relevant variables
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "
\^@a
\^?"
	     "
\^@  a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "
\^@  a
\^?"
	     "
\^@    a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "\^@
a
b
\^?"
	     "\^@
  a
  b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
\^@
a
b
\^?"
	     "*  x
\^@
   a
   b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
\^@
   a
   b
\^?"
	     "*  x
\^@
     a
     b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
   *  y
\^@
   a
   b
\^?"
	     "*  x
   *  y
\^@
      a
      b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 1)
	     "*  x
\^?
a
b
\^@"
	     "*  x
\^?
   a
   b
\^@"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 2)
	     "*  x
\^?
a
b
\^@"
	     "*  x
\^?
     a
     b
\^@"
	     t))
    ))

(ert-deftest rst-shift-region-left ()
  "Tests for `rst-shift-region' to the left."
  (let ((rst-indent-width 2)) ; Set relevant variables
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
\^@
     a
     b
\^?"
	     "*  x
\^@
   a
   b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "
\^@  a
\^?"
	     "
\^@a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "
\^@    a
\^?"
	     "
\^@  a
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "\^@
  a
  b
\^?"
	     "\^@
a
b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
\^@
   a
   b
\^?"
	     "*  x
\^@
a
b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
   *  y
\^@
      a
      b
\^?"
	     "*  x
   *  y
\^@
   a
   b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "*  x
\^?
   a
   b
\^@"
	     "*  x
\^?
a
b
\^@"
	     t))
    (should (equal-buffer
	     '(rst-shift-region 0)
	     "*  x
   *  y
\^@
      a
      b
\^?"
	     "*  x
   *  y
\^@
a
b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -1)
	     "\^@*  x
   *  y

      a
      b
\^?"
	     "\^@*  x
   *  y

      a
      b
\^?"
	     t))
    (should (equal-buffer
	     '(rst-shift-region -2)
	     "*  x
   *  y
\^@
        a
        b
\^?"
	     "*  x
   *  y
\^@
   a
   b
\^?"
	     t))
    ))
