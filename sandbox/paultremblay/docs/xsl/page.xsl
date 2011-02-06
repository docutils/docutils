<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->


    <xsl:template match= "xsl:attribute-set[@name='paper-size-simple-page-master']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">None</xsl:with-param> 
            <xsl:with-param name="docutils">/</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the paper size, used in other attribute sets.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='default-simple-page-master']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">None</xsl:with-param> 
            <xsl:with-param name="docutils">/</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the margins of the fo:body-region for all the pages.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='simple-page-master']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">5</xsl:with-param>
            <xsl:with-param name="text">Simple Page Master Sets</xsl:with-param>
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
        <block>- simple-page-master</block>
        <block>- first-simple-page-master</block>
        <block>- body-simpe-page-master</block>
        <block>- odd-simple-page-master</block>
        <block>- even-simple-page-master</block>
        <block>- toc-simple-page-master</block>
        <block>- toc-first-simple-page-master</block>
        <block>- toc-body-simple-page-master</block>
        <block>- toc-odd-simple-page-master</block>
        <block>- toc-even-simple-page-master</block>
        <block>- front-simple-page-master</block>
        <block>- front-first-simple-page-master</block>
        <block>- front-body-simple-page-master</block>
        <block>- front-odd-simple-page-master</block>
        <block>- front-even-simple-page-master</block>
        <block>
            These attriute sets format the margins of the
            fo:simple-page-master. By default, they inherit the
            ``'paper-size-simple-page-master'`` and
            ``'default-simple-page-master'`` attriute-sets, meaning each page
            will have identical size and margins, a satisfactory setup for
            many documents. However, the sizes and margins can be modified by
            page type, if desired.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='first-simple-page-master']|
        xsl:attribute-set[@name='body-simple-page-master']|
        xsl:attribute-set[@name='odd-simple-page-master']| 
        xsl:attribute-set[@name='even-simple-page-master']|
        xsl:attribute-set[@name='toc-simple-page-master']|
        xsl:attribute-set[@name='toc-first-simple-page-master']|
        xsl:attribute-set[@name='toc-body-simple-page-master']|
        xsl:attribute-set[@name='toc-odd-simple-page-master']| 
        xsl:attribute-set[@name='toc-even-simple-page-master']|
        xsl:attribute-set[@name='front-simple-page-master']|
        xsl:attribute-set[@name='front-first-simple-page-master']|
        xsl:attribute-set[@name='front-body-simple-page-master']|
        xsl:attribute-set[@name='front-odd-simple-page-master']| 
        xsl:attribute-set[@name='front-even-simple-page-master']
        " 
        priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='header-region-before']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:region-before</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/header</xsl:with-param> 
        </xsl:call-template>
        <block>
        The extent attribute specifies the header and footer height.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footer-region-after']" priority="3">
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
