<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    xmlns:date="http://exslt.org/dates-and-times"
    version="1.1">

    <xsl:template name="get-date">
        <xsl:param name="date"/>
          <xsl:choose>
              <xsl:when test="$date != ''">
                  <xsl:value-of select="$date"/>
              </xsl:when>
             <xsl:when test="function-available('date:date-time')">
                <xsl:value-of select="date:date-time()" />
             </xsl:when>
             <xsl:otherwise>
                 <xsl:message>
                     <xsl:text>No date available</xsl:text>
                 </xsl:message>
             </xsl:otherwise>
          </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
