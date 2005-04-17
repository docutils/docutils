;; Authors: Martin Blais <blais@furius.ca>
;; Date: $Date: 2005/04/01 23:19:41 $
;; Copyright: This module has been placed in the public domain.
;;
;; Regression tests for rest-adjust-section-title.
;;

(defvar rest-adjust-section-tests 
  '(
    (simple
"
Some Title@

"
"
Some Title
==========

")

    (simple-cursor-in-line
"
Some Tit@le

"
"
Some Title
==========

")

    (simple-cursor-beginning
"
@Some Title

"
"
Some Title
==========

")

    (simple-at-end-of-buffer
"
Some Title@"
"
Some Title
==========
")

    (cursor-on-empty-line-under
"
Some Title
@
"
"
Some Title
==========

")



    (partial
"
Some Title@
---
"
"
Some Title
----------

")

    (cursor-on-underline
"
Some Title
---@
"
"
Some Title
----------

")

    (cursor-on-underline-one-char
"
Some Title
~@
"
"
Some Title
~~~~~~~~~~

")

    (with-previous-text
"
Some Title
**********

Subtitle@

"
"
Some Title
**********

Subtitle
********

")

    (with-suggested-new-text
"
Some Title
==========

Subtitle
--------

Subtitle2@

"
"
Some Title
==========

Subtitle
--------

Subtitle2
~~~~~~~~~

"
2)

    (with-previous-text-rotating
"
Some Title
==========

Subtitle
--------

Subtitle2@

"
"
Some Title
==========

Subtitle
--------

Subtitle2
=========

"
3)

  )
  
  "A list of regression tests for the section update method.")
   

(defun regression-test-compare-expect-buffer (testlist fun)
  "Run the regression tests for the section adjusting method."
  
  (let ((buf (get-buffer-create "restructuredtext-regression-tests"))
	(specchar "@")
	)
    (dolist (curtest testlist)
      ;; prepare a buffer with the starting text, and move the cursor where
      ;; the special character is located
      (switch-to-buffer buf)
      (erase-buffer)
      (insert (cadr curtest))
      (search-backward specchar)
      (delete-char 1)
      
      ;; run the section title update command n times
      (dotimes (x (or (cadddr curtest) 1))
	(funcall fun))
      
      ;; compare the buffer output with the expected text
      (or (string=
	   (buffer-string)
	   (caddr curtest))
	  (progn
	    (error "Test %s failed." (car curtest))))
      )
    ))

;; evaluate this to run the tests, either interactively or in batch
(regression-test-compare-expect-buffer
 rest-adjust-section-tests
 (lambda ()
   (call-interactively 'rest-adjust-section-title)))
