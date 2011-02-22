import os, sys
num_styles = 30
en =  num_styles + 1
en = 3
ss = '/Users/cynthia/tmp/paultremblay/docutilsToFo/xsl_fo/table_extended_new.xsl'
write_obj = file(ss, 'w')
start= """<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id$ -->\n\n"""

write_obj.write(start)
# write attribute sets

for n in range(1,en):
    s = """\n     <xsl:attribute-set name="table%s-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

end = '</xsl:stylesheet>'
write_obj.write(end)

write_obj.close()
