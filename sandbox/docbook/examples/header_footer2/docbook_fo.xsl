<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>
    <!--creates a centered heading with no room for right or left headings-->
    <xsl:param name="header.column.widths">1 100 1</xsl:param>

   <xsl:template name="header.content">
      <xsl:param name="pageclass" select="''"/>
      <xsl:param name="sequence" select="''"/>
      <xsl:param name="position" select="''"/>
      <xsl:param name="gentext-key" select="''"/>
      <xsl:choose>
          <xsl:when test="$pageclass = 'body'  and $position = 'center'">
              <fo:block text-align="center">
                  <xsl:text >A very long heder that could contain chapter
                      title or other</xsl:text>
              </fo:block>
          </xsl:when>
      </xsl:choose>
   </xsl:template>  

</xsl:stylesheet>
