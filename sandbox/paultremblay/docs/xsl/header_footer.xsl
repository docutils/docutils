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
            the header and the body text, using ``'space-before = x'``
            and setting ``'space-before.conditionality'`` to ``'retain'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='paragraph-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the  paragraphs in the header. Use to set the space between
            the footer and the body text, using ``'space-before = x'``
            and setting ``'space-before.conditionality'`` to ``'retain'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='paragraph-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">decoration/footer/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the  footer. Use to set the space between
            the footer and the body text, using ``'space-before = x'``
            and setting ``'space-before.conditionality'`` to ``'retain'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='first-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='first-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the header for the first page.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='body-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='body-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the header of the body, when using a layout of 
            ``'first'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='odd-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='odd-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the header of odd pages, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='even-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='even-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the header of even pages, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='first-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='first-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the footer for the first page.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='body-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='body-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the footer of the body, when using a layout of 
            ``'first'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='odd-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='odd-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the footer of odd pages, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='even-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='even-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the footer of even pages, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>


    <xsl:template match= "xsl:attribute-set[@name='toc-first-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-first-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the header for the first page of the TOC.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-body-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-body-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the header of the body of the TOC, when using a layout of 
            ``'first'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-odd-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-odd-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the header of odd pages of the TOC, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-even-header-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-even-header']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the header of even pages of the TOC, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-first-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-first-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the footer for the first page of the TOC.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-body-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-body-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the footer of the body of the TOC, when using a layout of 
            ``'first'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-odd-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-odd-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the footer of odd pages of the TOC, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='toc-even-footer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container[@classes='toc-even-footer']</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the properties for the for the footer of even pages of the TOC, when using a layout of 
            ``'first-odd-even'``, or ``'odd-even'``.
        </block>
            
    </xsl:template>



</xsl:stylesheet>
