<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='header-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/header/paragraph[1]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the first paragrah in the header.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/header</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the  header. Use to set the space between
            the header and the body text, using ``'space-before = x'``
            and setting ``'space-before.conditionality'`` to ``'retain'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the  footer. Use to set the space between
            the footer and the body text, using ``'space-before = x'``
            and setting ``'space-before.conditionality'`` to ``'retain'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='header-second-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/header/paragraph[2]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the second paragrah in the header.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='header-third-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/header/paragraph[3]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the third paragrah in the header.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footer-first-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer/paragraph[1]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the first paragrah in the footer.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footer-second-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer/paragraph[2]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the second paragrah in the footer.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='footer-third-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer/paragraph[3]</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the third paragrah in the footer.
        </block>
    </xsl:template>

</xsl:stylesheet>
