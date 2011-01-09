<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <xsl:variable name="this-stylesheet" select="'page.xsl'"/>

    <xsl:attribute-set name="paper-size">
	<xsl:attribute name="page-width">8.5in</xsl:attribute>
	<xsl:attribute name="page-height">11in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="simple-page" use-attribute-sets="paper-size">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="first-page" use-attribute-sets="paper-size">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="body-page" use-attribute-sets="paper-size">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="odd-page" use-attribute-sets="paper-size">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="even-page" use-attribute-sets="paper-size">
	<xsl:attribute name="margin-left">1.0in</xsl:attribute>
	<xsl:attribute name="margin-right">1.0in</xsl:attribute>
	<xsl:attribute name="margin-top">1.0in</xsl:attribute>
	<xsl:attribute name="margin-bottom">1.0in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="page-header">
	<xsl:attribute name="extent">1.0in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:attribute-set name="page-footer">
	<xsl:attribute name="extent">.5in</xsl:attribute>
    </xsl:attribute-set>

    <xsl:variable name="extent">
        <xsl:value-of select="document('')/*/xsl:attribute-set[@name='page-header']/xsl:attribute[@name='extent']"/>
    </xsl:variable>

    <xsl:variable name="extent-bottom">
        <xsl:value-of select="document('')/*/xsl:attribute-set[@name='page-footer']/xsl:attribute[@name='extent']"/>
    </xsl:variable>

<!--
    <fo:page-number/>
    -->

    <!--
    TODO

    1. first page and everything else the same

    2. odd and even

    3 first, odd, and even

    Handle headers and footers

    5. Handle toc

    6. Handle the page wrapping for chapters.


    -->

    <xsl:template name="make-pages">
        <fo:layout-master-set>
            <xsl:call-template name="page-properties">
                <xsl:with-param name="page-layout" select="$page-layout"/> 
            </xsl:call-template>
            <xsl:call-template name="page-sequence">
                <xsl:with-param name="page-layout" select="$page-layout"/> 
            </xsl:call-template>
        </fo:layout-master-set>
    </xsl:template>

    <xsl:template name="make-simple-page">
        <fo:simple-page-master xsl:use-attribute-sets="simple-page" master-name="simple-page">  
            <xsl:element name="fo:region-body">
                <xsl:if test="document/decoration/header">
                    <xsl:attribute name="margin-top">
                        <xsl:value-of select="$extent"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test="document/decoration/footer">
                    <xsl:attribute name="margin-bottom">
                        <xsl:value-of select="$extent-bottom"/>
                    </xsl:attribute>
                </xsl:if>
            </xsl:element>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-first-page">
        <fo:simple-page-master xsl:use-attribute-sets="first-page" master-name="first">  
            <xsl:element name="fo:region-body">
                <xsl:if test="document/decoration/header and $suppress-first-page-header != 'True'">
                    <xsl:attribute name="margin-top">
                        <xsl:value-of select="$extent"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test="document/decoration/footer and $suppress-first-page-footer != 'True'">
                    <xsl:attribute name="margin-bottom">
                        <xsl:value-of select="$extent-bottom"/>
                    </xsl:attribute>
                </xsl:if>
            </xsl:element>

            <xsl:if test="document/decoration/header and $suppress-first-page-header != 'True'">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="first-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer and $suppress-first-page-footer != 'True'">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="first-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <!--for first page layout; this formats the rest of the pages-->
    <xsl:template name="make-body-page">
        <fo:simple-page-master xsl:use-attribute-sets="body-page" master-name="body">  
            <xsl:element name="fo:region-body">
                <xsl:if test="document/decoration/header">
                    <xsl:attribute name="margin-top">
                        <xsl:value-of select="$extent"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test="document/decoration/footer">
                    <xsl:attribute name="margin-bottom">
                        <xsl:value-of select="$extent-bottom"/>
                    </xsl:attribute>
                </xsl:if>
            </xsl:element>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name = "odd-even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name = "odd-even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-odd-page">
        <fo:simple-page-master xsl:use-attribute-sets="odd-page" master-name="odd">  
            <xsl:element name="fo:region-body">
                <xsl:if test="document/decoration/header">
                    <xsl:attribute name="margin-top">
                        <xsl:value-of select="$extent"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test="document/decoration/footer">
                    <xsl:attribute name="margin-bottom">
                        <xsl:value-of select="$extent-bottom"/>
                    </xsl:attribute>
                </xsl:if>
            </xsl:element>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="odd-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="odd-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="make-even-page">
        <fo:simple-page-master xsl:use-attribute-sets="even-page" master-name="even">  
            <xsl:element name="fo:region-body">
                <xsl:if test="document/decoration/header">
                    <xsl:attribute name="margin-top">
                        <xsl:value-of select="$extent"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test="document/decoration/footer">
                    <xsl:attribute name="margin-bottom">
                        <xsl:value-of select="$extent-bottom"/>
                    </xsl:attribute>
                </xsl:if>
            </xsl:element>
            <xsl:if test="document/decoration/header">
                <fo:region-before xsl:use-attribute-sets = "page-header" region-name="even-header"/>
            </xsl:if>
            <xsl:if test="document/decoration/footer">
                <fo:region-after xsl:use-attribute-sets = "page-footer" region-name="even-footer"/>
            </xsl:if>
        </fo:simple-page-master>
    </xsl:template>

    <xsl:template name="page-properties">
        <!--set up the physical properties of the pages-->
        <xsl:param name="page-layout"/>
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <xsl:call-template name="make-simple-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <xsl:call-template name="make-first-page"/>
                <xsl:call-template name="make-body-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <xsl:call-template name="make-odd-page"/>
                <xsl:call-template name="make-even-page"/>
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <xsl:call-template name="make-first-page"/>
                <xsl:call-template name="make-odd-page"/>
                <xsl:call-template name="make-even-page"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate="yes">
                    <xsl:value-of select="$this-stylesheet"/>
                    <xsl:text> Invalid parameter for param page-layout:"</xsl:text>
                    <xsl:value-of select="$page-layout"/>
                    <xsl:text>" Script now quitting</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="page-sequence">
        <xsl:param name="page-layout"/>
        <xsl:choose>
            <xsl:when test="$page-layout = '' or $page-layout = 'simple'">
                <fo:page-sequence-master master-name = "pages" >
                    <fo:repeatable-page-master-reference master-reference = "simple-page"/>
                </fo:page-sequence-master>
            </xsl:when>
            <xsl:when test="$page-layout = 'first'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "body" page-position = "rest"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'odd-even'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
            <xsl:when test="$page-layout = 'first-odd-even'">
                <fo:page-sequence-master master-name = "pages">
                    <fo:repeatable-page-master-alternatives>
                        <fo:conditional-page-master-reference master-reference = "first" page-position = "first"/>
                        <fo:conditional-page-master-reference master-reference = "odd" odd-or-even = "odd"/>
                        <fo:conditional-page-master-reference master-reference = "even" odd-or-even = "even"/>
                    </fo:repeatable-page-master-alternatives>
                </fo:page-sequence-master> 
            </xsl:when>
        </xsl:choose>
    </xsl:template>

</xsl:stylesheet>
