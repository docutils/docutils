verse writer
============

:Date: 2007-01-03

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
