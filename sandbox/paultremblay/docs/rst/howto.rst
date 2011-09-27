
Decreases space between header and text::

 <xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.1"
    >
    <xsl:import href="xsl_fo/docutils_to_fo.xsl"/>
    <xsl:attribute-set name="header-block">
        <xsl:attribute name="space-before">.5in</xsl:attribute>
    </xsl:attribute-set>

 </xsl:stylesheet>
