This directory contains parts of makefiles intended for inclusion in your
makefiles.

``Makelib.docutils`` contains only variables settings and implicit rules. 
It's designed to be maximally friendly to the makefile it's included in.

``Makefile.docutils`` includes the above file and builds upon it some useful
rules like ``html``, ``pdf`` or ``clean``.  The minimum example of a makefile
including it is::

    DOCS = foo.txt bar.txt
    
    include Makefile.docutils

However these makefiles are very flexible and configurable by setting many
variables.  They also support pre/postprocessing, notably with
`sandbox/cben/rolehack <../rolehack>`_ scripts and rudimentary
format-dependant convertion of ``.*`` names with sed scripts.

Enjoy and tell me (cben@users.sf.net) if you miss any functionality or
flexibility...
