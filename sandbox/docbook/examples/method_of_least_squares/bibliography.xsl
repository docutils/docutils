<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:attribute-set name="biblioentry.space">
        <xsl:attribute name="provisional-distance-between-starts">20mm</xsl:attribute>
        <xsl:attribute name="start-indent">0mm</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="biblioentry">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="bibliography.title.properties">
        <xsl:attribute name="font-size">14pt</xsl:attribute>
        <xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template match="d:bibliography">
        <xsl:apply-templates select="d:title"/>
        <fo:list-block xsl:use-attribute-sets="biblioentry.space">
            <xsl:apply-templates select="d:biblioentry"/>
        </fo:list-block>
    </xsl:template>

    <xsl:template match="d:bibliography/d:title">
          <xsl:variable name="id">
              <xsl:for-each select="..">
                    <xsl:call-template name="object.id"/>
              </xsl:for-each>
          </xsl:variable>
          <fo:block id = "{$id}" xsl:use-attribute-sets="bibliography.title.properties">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="d:biblioentry">
        <fo:list-item xsl:use-attribute-sets="biblioentry">
            <fo:list-item-label end-indent="label-end()" >
                <fo:block>
                    <xsl:text>[</xsl:text>
                    <xsl:value-of select="@xreflabel"/>
                    <xsl:text>]</xsl:text>
                </fo:block>
            </fo:list-item-label>
            <fo:list-item-body start-indent="body-start()">
                <fo:block>
                    <xsl:apply-templates select="d:authorgroup|d:author" mode="biblioentry"/>
                    <xsl:apply-templates select="d:title" mode="biblioentry"/>
                    <xsl:apply-templates select="d:edition" mode="biblioentry"/>
                    <xsl:apply-templates select="d:volumenum" mode="biblioentry"/>
                    <xsl:apply-templates select="d:publisher" mode="biblioentry"/>
                    <xsl:apply-templates select="d:date" mode="biblioentry"/>
                    <xsl:text>.</xsl:text>
                </fo:block>
            </fo:list-item-body>
        </fo:list-item>
    </xsl:template>

    <xsl:template match="d:authorgroup" mode="biblioentry">
        <xsl:apply-templates mode="biblioentry"/>
    </xsl:template>

    <xsl:template match="d:author" mode="biblioentry">
        <xsl:variable name="position">
            <xsl:number/>
        </xsl:variable>
        <xsl:variable name="join-text">
            <xsl:choose>
                <xsl:when test="$position = 1"/>
                <xsl:otherwise>
                    <xsl:text> and </xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$join-text"/>
        <xsl:apply-templates select="d:personname/d:firstname" mode="biblioentry"/>
        <xsl:text> </xsl:text>
        <xsl:apply-templates select="d:personname/d:surname" mode="biblioentry"/>
        <xsl:if test="position() = last()">
            <xsl:text>, </xsl:text>
        </xsl:if>
    </xsl:template>

    <xsl:template match="d:firstname|d:surname" mode="biblioentry">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="d:title" mode="biblioentry">
        <fo:inline font-style="italic">
            <xsl:apply-templates/>
        </fo:inline>
        <xsl:text>, </xsl:text>
    </xsl:template>

    <xsl:template match="d:edition" mode="biblioentry">
        <xsl:apply-templates/>
        <xsl:text> edition, </xsl:text>
    </xsl:template>

    <xsl:template match="d:volumenum" mode="biblioentry">
        <xsl:text>Vol. </xsl:text>
        <xsl:apply-templates/>
        <xsl:text>, </xsl:text>
    </xsl:template>

    <xsl:template match="d:publisher" mode="biblioentry">
        <xsl:apply-templates select="d:publishername" mode="biblioentry"/>
        <xsl:text>, </xsl:text>
        <xsl:apply-templates select="d:address" mode="biblioentry"/>
    </xsl:template>

    <xsl:template match="d:publishername" mode="biblioentry">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="d:publisher/d:address" mode="biblioentry">
        <xsl:apply-templates/>
        <xsl:text>, </xsl:text>
    </xsl:template>

    <xsl:template match="d:date" mode="biblioentry">
        <xsl:value-of select="substring(., 1,4)"/>
    </xsl:template>

</xsl:stylesheet>
