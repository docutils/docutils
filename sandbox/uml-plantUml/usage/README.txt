INFO: Generate UML diagrams from docutils using PlantUML
========================================================

Being able to generate html/pdf documents with diagrams from vi or automaticaly is great for me. Many many thanks to docutils and plantuml developers for sharing their vision and the effort!

ASK: mauriciocap@gmail.com / http://www.mauriciocap.com.ar 

SEE: http://plantuml.sourceforge.net/ Generate UML diagrams from text, runnable from a single jar!

SEE: http://docutils.sourceforge.net/ Conveniently write and generate documents in various output formats 

SEE: http://code.google.com/p/rst2pdf/ Generate PDF output without latex in windows 

Install:
--------
**WARNING:** the directive EXECUTES an external program with an argument passed from the rst input file! Don't use it if to process input files you don't trust.

#. replace/patch your docutils distribution with the files in src/, e.g. in my cygwin instance

::
	# cp -r src/* /usr/lib/python2.5/site-packages/docutils 

#. see usage examples in usage/ including how to setup defaults PlantUML

Examples:
---------

SEE: diagrams at http://plantuml.sourceforge.net/ , ALL should work as long as your plantuml installation works

USE the "uml" directive and write an indented block of PlantUML commands. No arguments are required, an image will be generated and inlined. The name is derived from the input line number, the generatd plantuml "source" is keep in the same directory.

.. uml::

	actor User
	participant "First Class" as A
	participant "Second Class" as B
	participant "Last Class" as C

	User -> A: DoWork
	activate A

	A -> B: << createRequest >>
	activate B

	B -> C: DoWork
	activate C
	C --> B: WorkDone
	destroy C

	B --> A: Request <u>Created</u>
	deactivate B

	A --> User: Done
	deactivate A

The same here:

.. uml::

	actor User
	participant "First Class" as A
	participant "Second Class" as B
	participant "Last Class" as C

	User -> A: DoWork
	activate A

You can pass a filename for the image (WITHOUT extension) as an optional parameter.

.. uml:: uml2/myDiagram123

	actor User
	participant "First Class" as A
	participant "Second Class" as B
	participant "Last Class" as C

	User -> A: DoMoreWork
	activate A

Development:
------------

This was a quick (but effective) hack :) In the near future I'd like to:

* implement the image generation as a "writer" instead, as docutils architecture suggest 

* contribute tests and better documentation

Please let me know how can I be of more help!

mauriciocap@gmail.com / http://www.mauriciocap.com.ar
