latex2e BUGS TODOS and other animals
------------------------------------

for processing tools/test.txt use pdflatex because it will handle png-images.

Rev. 2002-11-05

To do
~~~~~
mostly errors when processing tools/test.txt.

* abstract is not a docinfo entry so latex abstract is never used.

* linebreaking in docinfo adress is missing. html uses a pre, use flushleft ? 

* multiple author entries in docinfo (same thing as in html).

* dedication should be centered, text and title. 

* abstract title should be centered. 

* the indentional problematic error in test.txt is not referring anywhere.

* table cells with multirow and multicolumn

* enumeration too deep

* lists that donot start at one donot work.

* field list text is not indented.

* option list: is implemented as a table, but in html more like a description list.

* tables might be too wide

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

* ATTENTION:
  put labeling inside environments.
