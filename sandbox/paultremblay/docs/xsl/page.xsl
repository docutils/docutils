<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->


    <xsl:template match= "xsl:attribute-set[@name='paper-size']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">None</xsl:with-param> 
            <xsl:with-param name="docutils">/</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the paper size, used in other attribute sets.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='default-page-setup']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">None</xsl:with-param> 
            <xsl:with-param name="docutils">/</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the margins of the fo:body-region for all the pages.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='simple-page']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">3</xsl:with-param>
            <xsl:with-param name="text">Page Master Sets</xsl:with-param>
        </xsl:call-template>

        <block>
            <xsl:text>:fo: fo:simple-page-master</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: /</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:inherits: paper-size, default-page-setup</xsl:text> 
        </block>
        <block>
            The following attribute sets are identical:
        </block>
        <block>- simple-page</block>
        <block>- first-page</block>
        <block>- body-page</block>
        <block>- odd-page</block>
        <block>- even-page</block>
        <block>- toc-simple-page</block>
        <block>- toc-first-page</block>
        <block>- toc-body-page</block>
        <block>- toc-odd-page</block>
        <block>- toc-even-page</block>
        <block>- front-matter-simple-page</block>
        <block>- front-matter-first-page</block>
        <block>- front-matter-body-page</block>
        <block>- front-matter-odd-page</block>
        <block>- front-matter-even-page</block>
        <block>
            These attriute sets format the margins of the fo:simple-page-master. By default, they inherit 
            the paper-size and default-page-setup attriute-sets, meaning each page will have identical 
            size and margins, a satisfactory setup for many documents. However, the sizes and margins can be
            modified by page type, if desired.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='first-page']|
        xsl:attribute-set[@name='first-page']|
        xsl:attribute-set[@name='body-page']|
        xsl:attribute-set[@name='odd-page']| 
        xsl:attribute-set[@name='even-page']|
        xsl:attribute-set[@name='toc-simple-page']|
        xsl:attribute-set[@name='toc-first-page']|
        xsl:attribute-set[@name='toc-body-page']|
        xsl:attribute-set[@name='toc-odd-page']| 
        xsl:attribute-set[@name='toc-even-page']|
        xsl:attribute-set[@name='front-matter-simple-page']|
        xsl:attribute-set[@name='front-matter-first-page']|
        xsl:attribute-set[@name='front-matter-body-page']|
        xsl:attribute-set[@name='front-matter-odd-page']| 
        xsl:attribute-set[@name='front-matter-even-page']
        " 
        priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='page-header']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:region-before</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/header</xsl:with-param> 
        </xsl:call-template>
        <block>
        The extent attribute specifies the header and footer height.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='page-footer']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:region-after</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer</xsl:with-param> 
        </xsl:call-template>
        <block>
        The extent attribute specifies the header and footer height.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='front-matter-region-body']|
        xsl:attribute-set[@name='region-body']" priority="3"/>


</xsl:stylesheet>
