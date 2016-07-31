;; Tests for class `rst-Ttl'

(add-to-list 'load-path ".")
(load "init" nil t)
(init-rst-ert nil)

(ert-deftest rst-Ttl-new ()
  "Test `rst-Ttl-new'."
  (let* (;; "
	 ;; Simple
	 ;; ======
	 ;; 
	 ;; ----------
	 ;;   Double
	 ;; ----------
	 ;; 
	 ;;  Candidate
	 ;; 
	 ;; ~~~~~~~~~~
	 ;;    Half
	 ;; "
	 (ado-spl (rst-Ado-new-simple ?=))
	 (mtc-spl '(1 15 nil nil 1 8 9 14))
	 (ind-spl 0)
	 (hdr-spl (rst-Hdr-new ado-spl 0))
	 (txt-spl "Simple")
	 (lvl-spl 0)
	 (ado-dbl (rst-Ado-new-over-and-under ?-))
	 (mtc-dbl '(17 47 17 27 28 36 37 47))
	 (ind-dbl 2)
	 (hdr-dbl (rst-Hdr-new ado-dbl 2))
	 (txt-dbl "Double")
	 (lvl-dbl 1)
	 (ado-trn (rst-Ado-new-transition))
	 (mtc-trn '(49 59 nil nil 49 59 nil nil))
	 (ind-trn nil)
	 (txt-trn nil)
	 (ado-cnd nil)
	 (mtc-cnd '(49 59 nil nil 49 59 nil nil))
	 (ind-cnd 1)
	 (txt-cnd "Candidate")
	 (ado-hlf (rst-Ado-new-over-and-under ?~))
	 (mtc-hlf '(61 79 61 71 72 79 nil nil))
	 (ind-hlf 3)
	 (txt-hlf "Half"))

    ;; Check type checking of ado argument.
    (should-error (rst-Ttl-new hdr-spl mtc-spl ind-spl txt-spl)
		  :type 'wrong-type-argument)

    ;; Check type and value checking of match argument.
    (should-error (rst-Ttl-new ado-spl 1 ind-spl txt-spl)
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new ado-spl nil ind-spl txt-spl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-spl '(1 2 3) ind-spl txt-spl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-spl '(1 2 "3" 4 5 6 7 8) ind-spl txt-spl)
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new nil '(1 2 3 4 5 6 7 8) ind-spl txt-spl)
		  :type 'args-out-of-range)

    ;; Check value checking of match argument for tranisitions.
    (should-error (rst-Ttl-new ado-trn '(nil nil nil nil 1 8 9 14) ind-spl
			       txt-trn)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-trn '(1 15 1 8 9 14 nil nil) ind-spl txt-trn)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-trn '(1 15 nil nil 1 8 9 14) ind-spl txt-trn)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-trn '(1 15 nil nil nil nil 9 14) ind-spl
			       txt-trn)
		  :type 'args-out-of-range)

    ;; Check value checking of match argument for simple section header.
    (should-error (rst-Ttl-new ado-spl '(nil nil nil nil 1 8 9 14) ind-spl
			       txt-spl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-spl '(1 15 1 8 9 14 9 14) ind-spl txt-spl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-spl '(1 15 nil nil 1 8 nil nil) ind-spl
			       txt-spl)
		  :type 'args-out-of-range)

    ;; Check value checking of match argument for double section header.
    (should-error (rst-Ttl-new ado-dbl '(nil nil 17 27 28 36 37 47) ind-dbl
			       txt-dbl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-dbl '(17 47 nil nil 28 36 37 47) ind-dbl
			       txt-dbl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-dbl '(17 47 17 27 nil nil 37 47) ind-dbl
			       txt-dbl)
		  :type 'args-out-of-range)

    ;; Check type and value checking of indent argument.
    (should-error (rst-Ttl-new ado-trn mtc-trn 1 txt-trn)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-spl mtc-spl nil txt-spl)
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new ado-spl mtc-spl -1 txt-spl)
		  :type 'args-out-of-range)

    ;; Check type and value checking of text argument.
    (should-error (rst-Ttl-new ado-trn mtc-trn ind-trn "Text")
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-spl mtc-spl ind-spl nil)
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new ado-spl mtc-spl ind-spl 3)
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new ado-trn mtc-spl ind-spl txt-spl)
		  :type 'args-out-of-range)

    ;; Check type and value checking of hdr argument.
    (should-error (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl ado-spl)
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new ado-spl mtc-spl 1 txt-spl hdr-spl)
		  :type 'args-out-of-range)
    (should-error (rst-Ttl-new ado-dbl mtc-dbl ind-spl txt-dbl hdr-spl)
		  :type 'args-out-of-range)

    ;; Check type and value checking of level argument.
    (should-error (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl nil "1")
		  :type 'wrong-type-argument)
    (should-error (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl nil -1)
		  :type 'args-out-of-range)

    (should (rst-Ttl-p (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl
				    hdr-spl lvl-spl)))
    (should (rst-Ttl-p (rst-Ttl-new ado-dbl mtc-dbl ind-dbl txt-dbl
				    hdr-dbl lvl-dbl)))
    (should (rst-Ttl-p (rst-Ttl-new ado-cnd mtc-cnd ind-cnd txt-cnd)))
    (should (rst-Ttl-p (rst-Ttl-new ado-hlf mtc-hlf ind-hlf txt-hlf)))
    (should (rst-Ttl-p (rst-Ttl-new ado-trn mtc-trn ind-trn txt-trn)))
  ))

(ert-deftest rst-Ttl-evaluate-hdr ()
  "Test `rst-Ttl-evaluate-hdr'."
  (let* (;; "
	 ;; Simple
	 ;; ======
	 ;; 
	 ;; ----------
	 ;;   Double
	 ;; ----------
	 ;; 
	 ;;  Candidate
	 ;; 
	 ;; ~~~~~~~~~~
	 ;;    Half
	 ;; "
	 (ado-spl (rst-Ado-new-simple ?=))
	 (mtc-spl '(1 15 nil nil 1 8 9 14))
	 (ind-spl 0)
	 (hdr-spl (rst-Hdr-new ado-spl 0))
	 (txt-spl "Simple")
	 (lvl-spl 1)
	 (ado-dbl (rst-Ado-new-over-and-under ?-))
	 (mtc-dbl '(17 47 17 27 28 36 37 47))
	 (ind-dbl 2)
	 (hdr-dbl (rst-Hdr-new ado-dbl 2))
	 (txt-dbl "Double")
	 (lvl-dbl 2)
	 (ado-trn (rst-Ado-new-transition))
	 (txt-trn nil)
	 (ado-cnd nil)
	 (mtc-cnd '(49 59 nil nil 49 59 nil nil))
	 (ind-cnd 1)
	 (txt-cnd "Candidate")
	 (ado-hlf (rst-Ado-new-over-and-under ?~))
	 (mtc-hlf '(61 79 61 71 72 79 nil nil))
	 (ind-hlf 3)
	 (txt-hlf "Half"))
    (should (rst-Ttl-evaluate-hdr (rst-Ttl-new ado-spl mtc-spl ind-spl
					       txt-spl)))
    (should (rst-Ttl-evaluate-hdr (rst-Ttl-new ado-dbl mtc-dbl ind-dbl
					       txt-dbl)))
    (should (rst-Ttl-evaluate-hdr (rst-Ttl-new ado-hlf mtc-hlf ind-hlf
					       txt-hlf)))
    (should-not (rst-Ttl-evaluate-hdr (rst-Ttl-new ado-cnd mtc-cnd ind-cnd
						   txt-cnd)))
    (should-not (rst-Ttl-evaluate-hdr (rst-Ttl-new ado-trn mtc-cnd nil
						   txt-trn)))
    ))

(ert-deftest rst-Ttl-set-level ()
  "Test `rst-Ttl-set-level'."
  (let* (;; "
	 ;; Simple
	 ;; ======
	 ;; 
	 ;; ----------
	 ;;   Double
	 ;; ----------
	 ;; 
	 ;;  Candidate
	 ;; 
	 ;; ~~~~~~~~~~
	 ;;    Half
	 ;; "
	 (ado-spl (rst-Ado-new-simple ?=))
	 (mtc-spl '(1 15 nil nil 1 8 9 14))
	 (ind-spl 0)
	 (hdr-spl (rst-Hdr-new ado-spl 0))
	 (txt-spl "Simple")
	 (lvl-spl 1)
	 (ado-dbl (rst-Ado-new-over-and-under ?-))
	 (mtc-dbl '(17 47 17 27 28 36 37 47))
	 (ind-dbl 2)
	 (hdr-dbl (rst-Hdr-new ado-dbl 2))
	 (txt-dbl "Double")
	 (lvl-dbl 2)
	 (ado-trn (rst-Ado-new-transition))
	 (txt-trn nil)
	 (ado-cnd nil)
	 (mtc-cnd '(49 59 nil nil 49 59 nil nil))
	 (ind-cnd 1)
	 (txt-cnd "Candidate")
	 (ado-hlf (rst-Ado-new-over-and-under ?~))
	 (mtc-hlf '(61 79 61 71 72 79 nil nil))
	 (ind-hlf 3)
	 (txt-hlf "Half"))
    (should-error (rst-Ttl-set-level (rst-Ttl-new ado-spl mtc-spl ind-spl
						  txt-spl) -1)
		  :type 'args-out-of-range)

    (should (rst-Ttl-set-level (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl) 0))
    ))

(ert-deftest rst-Ttl-get-title-beginning ()
  "Test `rst-Ttl-get-title-beginning'."
  (let* (;; "
	 ;; Simple
	 ;; ======
	 ;; 
	 ;; ----------
	 ;;   Double
	 ;; ----------
	 ;; 
	 ;;  Candidate
	 ;; 
	 ;; ~~~~~~~~~~
	 ;;    Half
	 ;; "
	 (ado-spl (rst-Ado-new-simple ?=))
	 (mtc-spl '(1 15 nil nil 1 8 9 14))
	 (ind-spl 0)
	 (hdr-spl (rst-Hdr-new ado-spl 0))
	 (txt-spl "Simple")
	 (lvl-spl 1)
	 (ado-dbl (rst-Ado-new-over-and-under ?-))
	 (mtc-dbl '(17 47 17 27 28 36 37 47))
	 (ind-dbl 2)
	 (hdr-dbl (rst-Hdr-new ado-dbl 2))
	 (txt-dbl "Double")
	 (lvl-dbl 2)
	 (ado-trn (rst-Ado-new-transition))
	 (ado-cnd nil)
	 (mtc-cnd '(49 59 nil nil 49 59 nil nil))
	 (ind-cnd 1)
	 (txt-cnd "Candidate")
	 (ado-hlf (rst-Ado-new-over-and-under ?~))
	 (mtc-hlf '(61 79 61 71 72 79 nil nil))
	 (ind-hlf 3)
	 (txt-hlf "Half"))
    (should (equal (rst-Ttl-get-title-beginning
		    (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl)) 1))
    (should (equal (rst-Ttl-get-title-beginning
		    (rst-Ttl-new ado-dbl mtc-dbl ind-dbl txt-dbl)) 28))
    (should (equal (rst-Ttl-get-title-beginning
		    (rst-Ttl-new ado-hlf mtc-hlf ind-hlf txt-hlf)) 72))
    (should (equal (rst-Ttl-get-title-beginning
		    (rst-Ttl-new ado-cnd mtc-cnd ind-cnd txt-cnd)) 49))
    ))

(ert-deftest rst-Ttl-get-beginning_end ()
  "Test `rst-Ttl-get-beginning' and `rst-Ttl-get-end'."
  (let* (;; "
	 ;; Simple
	 ;; ======
	 ;; 
	 ;; ----------
	 ;;   Double
	 ;; ----------
	 ;; 
	 ;;  Candidate
	 ;; 
	 ;; ~~~~~~~~~~
	 ;;    Half
	 ;; "
	 (ado-spl (rst-Ado-new-simple ?=))
	 (mtc-spl '(1 15 nil nil 1 8 9 14))
	 (ind-spl 0)
	 (hdr-spl (rst-Hdr-new ado-spl 0))
	 (txt-spl "Simple")
	 (lvl-spl 1)
	 (ado-dbl (rst-Ado-new-over-and-under ?-))
	 (mtc-dbl '(17 47 17 27 28 36 37 47))
	 (ind-dbl 2)
	 (hdr-dbl (rst-Hdr-new ado-dbl 2))
	 (txt-dbl "Double")
	 (lvl-dbl 2)
	 (ado-trn (rst-Ado-new-transition))
	 (ado-cnd nil)
	 (mtc-cnd '(49 59 nil nil 49 59 nil nil))
	 (ind-cnd 1)
	 (txt-cnd "Candidate")
	 (ado-hlf (rst-Ado-new-over-and-under ?~))
	 (mtc-hlf '(61 79 61 71 72 79 nil nil))
	 (ind-hlf 3)
	 (txt-hlf "Half"))
    (should (equal (rst-Ttl-get-beginning
		    (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl)) 1))
    (should (equal (rst-Ttl-get-end
		    (rst-Ttl-new ado-spl mtc-spl ind-spl txt-spl)) 15))
    (should (equal (rst-Ttl-get-beginning
		    (rst-Ttl-new ado-dbl mtc-dbl ind-dbl txt-dbl)) 17))
    (should (equal (rst-Ttl-get-end
		    (rst-Ttl-new ado-dbl mtc-dbl ind-dbl txt-dbl)) 47))
    (should (equal (rst-Ttl-get-beginning
		    (rst-Ttl-new ado-hlf mtc-hlf ind-hlf txt-hlf)) 61))
    (should (equal (rst-Ttl-get-end
		    (rst-Ttl-new ado-hlf mtc-hlf ind-hlf txt-hlf)) 79))
    (should (equal (rst-Ttl-get-beginning
		    (rst-Ttl-new ado-cnd mtc-cnd ind-cnd txt-cnd)) 49))
    (should (equal (rst-Ttl-get-end
		    (rst-Ttl-new ado-cnd mtc-cnd ind-cnd txt-cnd)) 59))
    ))
