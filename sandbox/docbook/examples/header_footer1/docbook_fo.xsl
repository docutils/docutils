<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:import href="http://docbook.sourceforge.net/release/xsl-ns/current/fo/docbook.xsl"/>
    <xsl:param name="generate.toc">
        <!--
        To eliminate header and footer rules
    <xsl:param name="header.rule">0</xsl:param>
    <xsl:param name="footer.rule">0</xsl:param>
    -->
appendix  toc,title
article/appendix  nop
article   toc,title,figure,table
book      toc,title,figure,table,example,equation
chapter   toc,title
part      toc,title
preface   toc,title
qandadiv  toc
qandaset  toc
reference toc,title
sect1     toc
sect2     toc
sect3     toc
sect4     toc
sect5     toc
section   toc
set       toc,title
</xsl:param>

   <xsl:template name="header.content">
      <xsl:param name="pageclass" select="''"/>
      <xsl:param name="sequence" select="''"/>
      <xsl:param name="position" select="''"/>
      <xsl:param name="gentext-key" select="''"/>
      <xsl:choose>
          <xsl:when test="$pageclass = 'lot'">
              <fo:block >
                  <xsl:value-of select="$sequence"/>
                  <xsl:text >,</xsl:text>
                  <xsl:value-of select="$position"/>
                  <xsl:text >,</xsl:text>
                  <xsl:value-of select="$gentext-key"/>
              </fo:block>
          </xsl:when>
          <xsl:when test="$pageclass = 'body'  and $position = 'left'">
              <fo:block text-align="center">
                    <xsl:apply-templates select="." mode="title.markup"/>
              </fo:block>
          </xsl:when>
          <xsl:when test="$pageclass = 'body'  and $position = 'center'">
              <fo:block text-align="center">
                    <xsl:call-template name="gentext">
                    </xsl:call-template>
                    <xsl:apply-templates select="." mode="label.markup"/>
              </fo:block>
          </xsl:when>
          <xsl:otherwise >
              <fo:block>
                  <xsl:value-of select="$pageclass"/>
                  <xsl:text >,</xsl:text>
                  <xsl:value-of select="$sequence"/>
                  <xsl:text >,</xsl:text>
                  <xsl:value-of select="$position"/>
                  <xsl:text >,</xsl:text>
                  <xsl:value-of select="$gentext-key"/>
              </fo:block>
          </xsl:otherwise>
      </xsl:choose>
   </xsl:template>  

</xsl:stylesheet>
