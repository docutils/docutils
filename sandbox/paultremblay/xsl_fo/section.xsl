<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:attribute-set name="default-section-title-block">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
        <xsl:attribute name="keep-with-next">always</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level1-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">16</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level2-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">14</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level3-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">14</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level4-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">12</xsl:attribute>
        <xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level5-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">10</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level6-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">10</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level7-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">10</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level8-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">10</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level9-block" use-attribute-sets="default-section-title-block">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="font-size">10</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="title-number-inline">
        <xsl:attribute name="space-end">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:template name="make-ids">
        <xsl:param name="ids"/>
        <xsl:variable name = "id" select="normalize-space(substring-before($ids, ' '))"/>
        <xsl:if test = "$id != ''">
            <fo:inline id = "{$id}"/>
            <xsl:call-template name="make-ids">
                <xsl:with-param name="ids" select = "substring-after($ids, ' ')"/> 
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template match="section">
        <fo:block role="empty-section-for-id">
            <xsl:call-template name="make-ids">
                <xsl:with-param name="ids" select="concat(@ids, ' ')"/>
            </xsl:call-template>
        </fo:block>
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="section/title">
        <xsl:variable name="level">
            <xsl:value-of select="count(ancestor::section)"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <fo:block  role = "section1-title" xsl:use-attribute-sets="title-level1-block" id = "{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 2">
                <fo:block role="section2-title" xsl:use-attribute-sets="title-level2-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 3">
                <fo:block  role="section3-title" xsl:use-attribute-sets="title-level3-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 4">
                <fo:block role="section4-title" xsl:use-attribute-sets="title-level4-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 5">
                <fo:block  role="section5-title" xsl:use-attribute-sets="title-level5-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 6">
                <fo:block role="section6-title" xsl:use-attribute-sets="title-level6-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 7">
                <fo:block role="section7-title" xsl:use-attribute-sets="title-level7-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 8">
                <fo:block role="section8-title"  xsl:use-attribute-sets="title-level8-block" id ="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 9">
                <fo:block role="section9-title"  xsl:use-attribute-sets="title-level9-block" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate="yes">
                    <xsl:text>Can't handle sections more than 9 deep.</xsl:text>
                    <xsl:text>Processing XSLT quitting.</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template name="format-number">
        <xsl:param name="string"/>
        <xsl:param name="level">1</xsl:param>
        <xsl:variable name="num">
            <xsl:value-of select="substring-before($string, '.')"/>
        </xsl:variable>
        <xsl:if test = "normalize-space($num) != ''">
            <xsl:choose>
                <xsl:when test="$level=1">
                    <xsl:number value="$num" format="{$number-section1}"/>
                </xsl:when>
                <xsl:when test="$level=2">
                    <xsl:number value="$num" format="{$number-section2}"/>
                </xsl:when>
                <xsl:when test="$level=3">
                    <xsl:number value="$num" format="{$number-section3}"/>
                </xsl:when>
                <xsl:when test="$level=4">
                    <xsl:number value="$num" format="{$number-section4}"/>
                </xsl:when>
                <xsl:when test="$level=5">
                    <xsl:number value="$num" format="{$number-section5}"/>
                </xsl:when>
                <xsl:when test="$level=6">
                    <xsl:number value="$num" format="{$number-section6}"/>
                </xsl:when>
                <xsl:when test="$level=7">
                    <xsl:number value="$num" format="{$number-section7}"/>
                </xsl:when>
                <xsl:when test="$level=8">
                    <xsl:number value="$num" format="{$number-section8}"/>
                </xsl:when>
                <xsl:when test="$level=9">
                    <xsl:number value="$num" format="{$number-section9}"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$num"/>
                </xsl:otherwise>
            </xsl:choose> 
            <xsl:call-template name="format-number">
                <xsl:with-param name="level" select="$level + 1"/>
                <xsl:with-param name="string" select="substring-after($string, '.')"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template match="section/title/generated[@classes='sectnum']">
        <xsl:variable name="num" select="concat(substring-before(., '&#x00a0;'), '.')"/>
        <fo:inline xsl:use-attribute-sets="title-number-inline" role="number">
            <xsl:call-template name="format-number">
                <xsl:with-param name="string" select="$num"/>
            </xsl:call-template>
        </fo:inline>
    </xsl:template>



    
</xsl:stylesheet>
