latex2e BUGS TODOS and other animals
------------------------------------

Rev. 2002-09-30

To do
~~~~~

* check indentation in verbatim is right.


* additional docinfo items: the field_body is inserted as text.

* docinfo item names must be translated.  
  author, date, ... use self.language.labels

  docinfo environment tabular fails on long lines.
  tabualrx, description, ...
	
*	knowing that the latex code will be used for pdf generation would allow
  to set pdf document information.

  for pdflatex package times gives smaller files than paladino.

  but setting this automatic might result in including several font packages.

* if latex does not number the sections (section*) pdflatex does not generate
  bookmarks on the table of contents entries.

  including not autonumbered section in the toc:

  not printing numbers into section headings, but latex does indent as if there
  were numbers::

    \renewcommand{\thesection}{}
    \renewcommand{\thesubsection}{}
    \renewcommand{\thesubsubsection}{}


* switch for autonumbersections (section or section*) could be set automatically
  to of if the first section starts with a number.

* ATTENTION:
  put labeling inside environments.
