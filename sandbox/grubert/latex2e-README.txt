latex2e BUGS TODOS and other animals
------------------------------------

Rev. 2002-09-28

* converting docutils/README.txt::

    3. Install the package::
          
          <path_to_python.exe>\python setup.py install
  
  the backslash was not escaped, but as i do it becomes a newline.
  We might need a latin-1 encoding of the text and put it into
  verbatim.

* additional docinfo items: the field_body is inserted as text.

* docinfo item names must be translated.  
  author, date, ...
	
*	knowing that the latex code will be used for pdf generation would allow
  to set pdf document information.

  for pdflatex package times gives smaller files than paladino.

  but setting this automatic might result in including several font packages.

* if latex does not number the sections (section*) pdflatex does not generate
  bookmarks on the table of contents entries.

* switch for autonumbersections (section or section*) could be set automatically
  to of if the first section starts with a number.

