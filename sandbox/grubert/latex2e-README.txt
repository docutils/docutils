latex2e BUGS TODOS and other animals
------------------------------------

for processing tools/test.txt use pdflatex because it will handle png-images.


:Author: engelbert gruber
:Contact: grubert@users.sourceforge.net
:Date: $Date$

To do
~~~~~

* handle image options : height,width,class.

* setting urls: french does put a spcae before ":" and "?".

  using ``\url`` long urls the url is typeset int tt, but hyphenates.
  using ``\href`` does typeset in normal font.

* dedication is centered italic in html (test.txt).

* term: html puts classifier in italic and newline after classifier,
  latex in parentheses and no newline after it (test.txt).

* field lists are indented in latex (test.txt).

* literal block is indented in html (test.txt).

* doctest block is indented in html.

* footnotes donot link back in latex.

* quotes for german: {\glqq} {\grqq} {\glq} {\grq} {\dq}.

* support embed-stylesheet.

* the ^-sign is problematic: using mathmode wedge is usually the wrong font.

* Maybe add end of line after term in definition list. see
    http://roundup.sf.net/doc-0.5/features.pdf

* In tools.txt the option tables right column, there should be some more spacing
  between the description and the next paragraph "Default:".

  Paragraph separation in tables is hairy. 
  see http://www.tex.ac.uk/cgi-bin/texfaq2html?label=struttab

  - The strut solution did not work.
  - setting extrarowheight added ad top of row not between paragraphs in
    a cell. ALTHOUGH i set it to 2pt because, text is too close to the topline.
  - baselineskip/stretch does not help.

* two hlines after table head and on table end ?

* table: multicol cells are always {l}.

* pdfbookmark level 4 (and greater) does not work (might be settable but OTOH).

* center subsection{Abstract} gives a latex error here.
  ``! LaTeX Error: Something's wrong--perhaps a missing \item.``
  Committed a HACK: centering by hfill

* document errors are also too silent.

* use optionlist for docinfo.

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

* tabularx says "do not use any multicolmn which spans any X column.
  maybe use ltxtable instead of tabularx (Longtable combined with tabularx).
  but ltxtable disables longtable's multicolumn.

* multiple author entries in docinfo (same thing as in html).

* the indentional problematic error in test.txt is not referring anywhere.

* table cells with multirow and multicolumn

* tables have borders, and the border is missing in empty cells.

* footnotes are not all on the same page (as in tools/test.txt) and do not link back
  and forth.

* no link to system errors.

* hyperlinks are not hyphenated this leads to bad spacing. see tools/test.txt 
  2.14 directives directives

* meta keywords into pdf ?

* pagestyle headings does not work, when sections are starred.

* additional docinfo items: the field_body is inserted as text.

ATTENTION
~~~~~~~~~
* put labeling inside (after the begin) environments.
* me (e.g.) uses textwidth, julien linewidth see what is what.

  textwidth: is the normal width of the text on a page. It should generally 
  be changed only in the preamble. 

  linewidth: is the width of lines in the current environment. Normally equal to
    \textwidth, it may be different within an environment such as list or quote 
    environments.

* from http://netra.wustl.edu/links/LaTeX/LaTeX.html

  - Aligning Images 

    By default, images align according to their "reference point," which is never 
    what you want it to be. Put a minipage around an image to make it align 
    properly (according to its center)::

      \usepackage{graphicx}
      \begin{minipage}{2in}
        \includegraphics[width=2in,angle=-90]{foo}
      \end{minpage}

  - 2-Column Images 

    When in 2-column mode (using either \twocolumn or the multicols environment 
    (package multicol)), use \begin{figure*} and \begin{table*} to create figures
    and tables that span the entire width of the page. \begin{figure} and 
    \begin{table} span only one column.

  - LaTeX->PDF 

    I've found that the best way to convert from LaTeX to PDF is to use "pdflatex."
    The first time that you run it, you'll find that the produced PDF file doesn't
    include any of your figures.  To fix this, 

    1. Use "epstopdf" to convert all of your EPS images to PDF 
    2. Make sure that you are using the "graphicx" package for including images.
    3. Remove any .eps or .ps extensions from filenames in your LaTeX file 

    When you run LaTeX, graphicx will use the dvips driver to incorporate EPS 
    files. When you run pdflatex, it will pick up the pdftex driver and incorporate
    the PDF files.
