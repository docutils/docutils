latex2e BUGS TODOS and other animals
------------------------------------

for processing tools/test.txt use pdflatex because it will handle png-images.

Rev. 2002-11-12

To do
~~~~~

* consider peter funk's hooks for TeXpert:
  
  * Define his own document preamble (including the choice to
    choose his own documentclass.  That would make the ``--documentclass``
    option superfluous).  I suggest to call this option ``--preamble``

  * Use two additional hooks to put additional stuff just behind the 
    ``\begin{document}`` and just before the ``\end{document}`` macros.
    Typical uses would be ``\tableofcontents``, ``\listoffigures`` and
    ``\appendix``, ``\makeindex``, ``\makeglossary`` and some such 
    for larger documents.

* sometimes tables have no thead ? (local test unlaut)

* table heads and footer for longtable (firstpage lastpage ..)


* ordered list numbering style is taken from latex a.,b. in source
  gives 1., .. in document.


* use table columnwidth.

* pep headers come in different ?

* tabularx says "do not use any multicolmn which spans any X column.
  maybe use ltxtable instead of tabularx (Longtable combined with tabularx).
  but ltxtable disables longtable's multicolumn.

* abstract is not a docinfo entry so latex abstract is never used.

* multiple author entries in docinfo (same thing as in html).

* dedication should be centered, text and title. 

* abstract title should be centered. 

* the indentional problematic error in test.txt is not referring anywhere.

* table cells with multirow and multicolumn

* enumeration too deep: latex goes up to four.
	
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

* footnotes are different (what is a footnote for a html document, and why does it have
  more than one footer ?)

* leading spaces in line-blocks look squeezed 	

* document errors are also too silent.

* footnotes are spread vertically ?
* meta keywords into pdf ?

* pagestyle headings does not work, when sections are starred.

* latex does not hyphenate here. 

* additional docinfo items: the field_body is inserted as text.

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

* multipage table
* field lists as description inside quote.
* literal blocks are inside a quote environment (means indented).
