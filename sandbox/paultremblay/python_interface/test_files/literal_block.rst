.. an example of code

.. $Id$

Python code is below::

 def make_attribute_set(the_name, the_dict):
     sys.stdout.write('<xsl:attribute-set name="%s">\n' % (the_name))
     the_keys = the_dict.keys()
     the_keys.sort()
     for the_key in the_keys:
         sys.stdout.write('     <xsl:attribute name="%s">' % (the_key))
         sys.stdout.write(the_dict[the_key])
         sys.stdout.write('</xsl:attribute>\n')
     sys.stdout.write('</xsl:attribute-set>\n\n')
 sys.stdout.write("""
 <xsl:stylesheet 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:fo="http://www.w3.org/1999/XSL/Format"
     version="1.1"
     >
     <xsl:import href="%s"/>
     """ % (stylesheet))
