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

    <xsl:template name="make-id">
        <xsl:variable name="current-id">
            <xsl:call-template name="find-id"/>
        </xsl:variable>
        <xsl:variable name="preceding-refid">
            <xsl:for-each select="preceding-sibling::*[1]">
                <xsl:if test="self::target">
                    <xsl:value-of select="@refid"/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:choose >
            <xsl:when test="self::table"><!--tables can only have ids in title-->
                <xsl:if test="title/target">
                    <xsl:attribute name="xml:id">
                        <xsl:call-template name="find-id">
                            <xsl:with-param name="string" select="title/target[@ids]"/>
                        </xsl:call-template>
                    </xsl:attribute>
                </xsl:if>
            </xsl:when>
            <xsl:when test="$preceding-refid != ''">
                <xsl:if test="$preceding-refid != $current-id">
                    <xsl:message terminate="yes">
                        <xsl:text >This shouldn't happen: preceding id of "</xsl:text>
                        <xsl:value-of select="$preceding-refid"/>
                        <xsl:text > does not match current id of " </xsl:text>
                        <xsl:value-of select="$current-id"/>
                    </xsl:message>
                </xsl:if>
                <xsl:attribute name="xml:id">
                    <xsl:value-of select="$preceding-refid"/>
                </xsl:attribute>
            </xsl:when>
            <xsl:when test="$current-id != ''">
                <xsl:attribute name="xml:id">
                    <xsl:value-of select="$current-id"/>
                </xsl:attribute>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="find-id">
        <xsl:param name="string"/>
        <xsl:variable name="id-string">
            <xsl:choose >
                <xsl:when test="$string != ''">
                    <xsl:value-of select="$string"/>
                </xsl:when>
                <xsl:otherwise >
                    <xsl:value-of select="@ids"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose >
            <xsl:when test="contains($id-string, ' ')">
                <xsl:value-of select="substring-after($id-string, ' ')"/>
            </xsl:when>
            <xsl:otherwise >
                <xsl:value-of select="$id-string"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
