verse writer
============

:Date: 2007-01-03

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
