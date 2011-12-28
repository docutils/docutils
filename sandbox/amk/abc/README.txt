
The abc.py module implements a Docutils directive that accepts content
written in the ABC language for musical notation
(http://www.gre.ac.uk/~c.walshaw/abc/). 

The directive relies on a number of libraries and external tools.
Requirements:

  * abcm2ps (http://moinejf.free.fr/), which turns ABC into PostScript.
  * Ghostscript (http://www.cs.wisc.edu/~ghost/), which turns 
    PostScript into PNG files.
  * The Python Imaging Library (used to crop the PNG files by removing lots of 
    blank space).

Images are converted to PNGs and written to the document output
directory.

--amk
