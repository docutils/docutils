verse writer
============

:Date: 2007-01-04

rst2verse is a LaTeX-writer to transfer single file poems into 
LaTeX. The generated files are assumed to be included into a master 
LaTeX document.

Usage :

  rst2vers.py input output

simple transformation from a reStructured text into a LaTeX 
verse poem.

reSt input ::

  All My Exes
  ---------------

  | your dogs, your daughters
  | or the state of your kitchen.
  | 
  | I will live in the secure orbit

output ::

  \poemtitle{All My Exes}

  \begin{verse}
  your dogs, your daughters \\
  or the state of your kitchen. \\
  
  I will live in the secure orbit \\

  \end{verse}

verse commands
--------------

* ``\poemtitle{TITEL}``  poems title. Similar to ``\section`` commands
  this is included in a table of contents.
* ``\\``   new line in verse.
* ``\\!``  end of last line in a stanza. Adds a separating line.
* ``\vin`` indent line by 1,5 em.

Open issues
-----------

* Inlcude into the standard latex-writer to be able to set poems with
  surrounding text. This would require a option to use verse for 
  literal-blocks and to use poemtitle if the section contains only
  a literal-block (poem).
* Only one ``\vin`` level is supported.
