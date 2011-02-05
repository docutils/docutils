<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='default-page-sequence']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:page-sequence</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the all of the document.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='front-page-sequence']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:page-sequence</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
            <xsl:with-param name="inherits">default-page-sequence</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the complete run of pages, in this case, the front matter.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-page-sequence']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:page-sequence</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
            <xsl:with-param name="inherits">default-page-sequence</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the complete run of pages, in this case, the toc and any
            pages associated with it.
        </block>
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='body-page-sequence']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:page-sequence</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
            <xsl:with-param name="inherits">default-page-sequence</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the complete run of pages, in this case, the body.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='default-flow']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:flow</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the default attributes for the front-flow, toc-flow, 
            and body-flow
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='front-flow']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:flow</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
            <xsl:with-param name="inherits">default-flow</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties of the body in the front sequence of pages. Since 
            the front sequence has no headers and footers, that means everything.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-flow']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:flow</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
            <xsl:with-param name="inherits">default-flow</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties of the body in the toc sequence of pages, which
            means everything except headers and footers.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='body-flow']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:flow</xsl:with-param> 
            <xsl:with-param name="docutils">document</xsl:with-param> 
            <xsl:with-param name="inherits">default-flow</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties of the body in the body sequence of pages, which
            means everything except headers and footers.
        </block>
    </xsl:template>



    
</xsl:stylesheet>

