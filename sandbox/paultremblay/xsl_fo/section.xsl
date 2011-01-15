<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:attribute-set name="title-level1">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level2">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level3">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level4">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level5">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level6">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level7">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level8">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-level9">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>


    <xsl:attribute-set name="title-number">
        <xsl:attribute name="space-end">12pt</xsl:attribute>
    </xsl:attribute-set>

    <!--do nothing for sections, at least for now-->
    <xsl:template match="section">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="section/title">
        <xsl:variable name="level">
            <xsl:value-of select="count(ancestor::section)"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$level = 1">
                <fo:block  xsl:use-attribute-sets="title-level1" id = "{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 2">
                <fo:block  xsl:use-attribute-sets="title-level2" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 3">
                <fo:block  xsl:use-attribute-sets="title-level3" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 4">
                <fo:block  xsl:use-attribute-sets="title-level4" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 5">
                <fo:block  xsl:use-attribute-sets="title-level5" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 6">
                <fo:block  xsl:use-attribute-sets="title-level6" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 7">
                <fo:block  xsl:use-attribute-sets="title-level7" id="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 8">
                <fo:block  xsl:use-attribute-sets="title-level8" id ="{@refid}">
                    <xsl:apply-templates/>
                </fo:block>
            </xsl:when>
            <xsl:when test="$level = 9">
                <fo:block  xsl:use-attribute-sets="title-level9" id="{@refid}">
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
        <fo:inline xsl:use-attribute-sets="title-number" role="number">
            <xsl:call-template name="format-number">
                <xsl:with-param name="string" select="$num"/>
            </xsl:call-template>
        </fo:inline>
    </xsl:template>



    
</xsl:stylesheet>
