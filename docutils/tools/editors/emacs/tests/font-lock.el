;; Tests for font-locking code

(add-to-list 'load-path ".")
(load "ert-support" nil t)

(ert-deftest rst-forward-indented-block ()
  "Tests `rst-forward-indented-block'."
  (should (equal-buffer-return
	   '(rst-forward-indented-block)
	   (concat buf-point-char "abc")
	   (concat buf-point-char "abc")
	   nil))
  (should (equal-buffer-return
	   '(rst-forward-indented-block)
	   (concat "  " buf-point-char "abc

def")
	   (concat "  abc
" buf-point-char "
def")
	   7))
  )
