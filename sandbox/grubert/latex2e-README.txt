latex2e BUGS TODOS and other animals
------------------------------------

Rev. 2002-09-26

* converting docutils/README.txt::

    3. Install the package::
          
          <path_to_python.exe>\python setup.py install
  
  the backslash was not escaped, but as i do it becomes a newline.
  We might need a latin-1 encoding of the text and put it into
  verbatim.

* additional docinfo items: the field_body is inserted as text.

* docinfo item names must be translated.  
