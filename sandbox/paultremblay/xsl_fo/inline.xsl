<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:ml = "http://www.w3.org/1998/Math/MathML"
    version="1.1">

    <!-- $Id: inline.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->

    <xsl:attribute-set name="emphasis-inline" >
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="strong-inline" >
	<xsl:attribute name="font-weight">bold</xsl:attribute>
    </xsl:attribute-set>

    <!--attribute set for reference elements with refuri attriute. Element is fo:basic-link. `docutils`-->
    <xsl:attribute-set name="basic-link-inline" >
	<xsl:attribute name="text-decoration">underline</xsl:attribute>
        <xsl:attribute name="color">blue</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="literal-inline">
        <xsl:attribute name="font-family">monospace</xsl:attribute>
        <xsl:attribute name="font-size">8</xsl:attribute>
        <xsl:attribute name="white-space">pre</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="title-reference-inline" >
	<xsl:attribute name="font-style">italic</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="subscript">
        <xsl:attribute name="vertical-align">sub</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="superscript">
        <!--
        <xsl:atribute name="baseline-shift">super</xsl:atribute>
        -->
        <xsl:attribute name="vertical-align">super</xsl:attribute>
    </xsl:attribute-set>


    <xsl:template match="strong">
        <fo:inline xsl:use-attribute-sets="strong-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="emphasis">
        <fo:inline xsl:use-attribute-sets="emphasis-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template name="reference-refid">
        <xsl:choose>
            <xsl:when test="$internal-link-type = 'link'">
                <fo:inline>
                    <fo:basic-link xsl:use-attribute-sets="basic-link-inline" internal-destination="{@refid}">
                        <xsl:apply-templates/>
                    </fo:basic-link>
                </fo:inline>
            </xsl:when>
            <xsl:when test="$internal-link-type = 'page'">
                <fo:page-number-citation ref-id="{@refid}"/>
            </xsl:when>
            <xsl:when test="$internal-link-type = 'page-link'">
                <fo:inline>
                    <fo:basic-link xsl:use-attribute-sets="basic-link-inline" internal-destination="{@refid}">
                        <fo:page-number-citation ref-id="{@refid}"/>
                    </fo:basic-link>
                </fo:inline>
            </xsl:when>
        </xsl:choose>
        
    </xsl:template>

    <!--internal links-->
    <xsl:template match="reference[@refid]">
        <xsl:choose>
            <xsl:when test="ancestor::paragraph">
                <xsl:call-template name="reference-refid"/>
            </xsl:when>
            <xsl:otherwise>
                <fo:block>
                    <xsl:call-template name="reference-refid"/>
                </fo:block>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--this template creates clickable links; you may want to give the option to turn this off-->
    <xsl:template match= "reference[@refuri]">
        <fo:basic-link xsl:use-attribute-sets="basic-link-inline" external-destination="url('{@refuri}')"> 
            <xsl:apply-templates/>
        </fo:basic-link>
    </xsl:template> 

    <xsl:template match="target">
        <xsl:if test="parent::paragraph">
            <fo:inline id="{@ids}">
                <xsl:apply-templates/>
            </fo:inline>
        </xsl:if>
    </xsl:template>

    <xsl:template match="literal">
        <fo:inline xsl:use-attribute-sets="literal-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="title_reference">
        <fo:inline xsl:use-attribute-sets="title-reference-inline">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="inline[@classes]">
        <xsl:variable name="msg">
            <xsl:text>Don't know what to do with inline with classes "</xsl:text>
            <xsl:value-of select="@classes"/>
            <xsl:text>"</xsl:text>
        </xsl:variable>
        <xsl:call-template name="error-message">
            <xsl:with-param name="msg" select="$msg"/>
        </xsl:call-template>
        
    </xsl:template>

    <xsl:template match="subscript">
        <fo:inline role="subscript" xsl:use-attribute-sets = "subscript">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <xsl:template match="superscript">
        <fo:inline role="superscript" xsl:use-attribute-sets = "superscript">
            <xsl:apply-templates/>
        </fo:inline>
    </xsl:template>

    <!--Change this if there is MathML-->
    <xsl:template match="paragraph/math">
        <xsl:choose>
            <xsl:when test="descendant::ml:math">
                <fo:inline >
                    <fo:instream-foreign-object>
                        <xsl:copy-of select="ml:math"/>
                    </fo:instream-foreign-object>
                </fo:inline>
            </xsl:when>
            <xsl:otherwise>
                <fo:inline xsl:use-attribute-sets="literal-inline">
                    <xsl:apply-templates/>
                </fo:inline>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    


    
</xsl:stylesheet>


