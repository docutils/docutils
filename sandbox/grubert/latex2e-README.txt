latex2e BUGS TODOS and other animals
------------------------------------

for processing tools/test.txt use pdflatex because it will handle png-images.


Rev. 2002-12-09

To do
~~~~~

* center subsection{Abstract} gives a latex error here.
  ``! LaTeX Error: Something's wrong--perhaps a missing \item.``

* the title "table of contents" is centered too.

* consider peter funk's hooks for TeXpert:
  
  * Define his own document preamble (including the choice to
    choose his own documentclass.  That would make the ``--documentclass``
    option superfluous).  I suggest to call this option ``--preamble``

  * Use two additional hooks to put additional stuff just behind the 
    ``\begin{document}`` and just before the ``\end{document}`` macros.
    Typical uses would be ``\tableofcontents``, ``\listoffigures`` and
    ``\appendix``, ``\makeindex``, ``\makeglossary`` and some such 
    for larger documents.

* hyphens: co-developers should be co--developers ?

* table heads and footer for longtable (firstpage lastpage ..)

* longtable does not work with multirow

* ordered list numbering style is taken from latex a.,b. in source
  gives 1., .. in document.


* table width is always full line width.
  how to make narrower tables.

* pep headers come in different ?

* tabularx says "do not use any multicolmn which spans any X column.
  maybe use ltxtable instead of tabularx (Longtable combined with tabularx).
  but ltxtable disables longtable's multicolumn.

* multiple author entries in docinfo (same thing as in html).

* the indentional problematic error in test.txt is not referring anywhere.

* table cells with multirow and multicolumn

	
* lists that donot start at one donot work.

* option list: is implemented as a table, but in html more like a description list.

* tables have borders, and the border is missing in multicol cells. or empty cells.

* footnotes are not all on the same page (as in tools/test.txt) and do not link back
  and forth.

* no link to system errors.	

* hyperlinks are not hyphenated this leads to bad spacing. see tools/test.txt 
  2.14 directives directives

* latex might shift images (or figures) to other pages. this is latex. but then we 
  need a see figure 1 note in the text.

* admonitions are not so visible as in html (make a border a bigger title, indent
  text)

* leading spaces in line-blocks look squeezed 	

* document errors are also too silent.

* meta keywords into pdf ?

* pagestyle headings does not work, when sections are starred.

* latex does not hyphenate here. 

* additional docinfo items: the field_body is inserted as text.

* footnotes are spread vertically (not with juliens style.tex)
  so include always.

* for pdflatex package times gives smaller files than paladino.

  but setting this automatic might result in including several font packages.

ATTENTION
~~~~~~~~~
* put labeling inside (after the begin) environments.
* me (e.g.) uses textwidth, julien linewidth see what is what.

  textwidth: is the normal width of the text on a page. It should generally 
  be changed only in the preamble. 

  linewidth: is the width of lines in the current environment. Normally equal to
    \textwidth, it may be different within an environment such as list or quote 
    environments.

Done
~~~~

* enumeration too deep: latex goes up to four.
* abstract title should be centered. 
* docinfo table centered and narrower.
* bibliographic field names have a ":".
* german (de*) quotes.
* colspec for tables without heads.
* multipage table
* field lists as description inside quote.
* literal blocks are inside a quote environment (means indented).
