<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

      <xsl:variable name="region-body-margin-top">
          <xsl:choose>
              <xsl:when test="/document/decoration/header">
                  <xsl:text>.75in</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                  <xsl:text>0in</xsl:text>
              </xsl:otherwise>
          </xsl:choose>
      </xsl:variable>
      <xsl:variable name="region-body-margin-bottom">
          <xsl:choose>
              <xsl:when test="/document/decoration/footer">
                  <xsl:text>.75in</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                  <xsl:text>0in</xsl:text>
              </xsl:otherwise>
          </xsl:choose>
      </xsl:variable>
    
</xsl:stylesheet>
