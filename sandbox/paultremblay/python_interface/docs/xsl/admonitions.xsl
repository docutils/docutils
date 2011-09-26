<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">
    
    <!-- $Id$ -->

    <xsl:template match= "xsl:attribute-set[@name='default-admonition-outer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">block</xsl:with-param> 
            <xsl:with-param name="docutils">None</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the outer blocks of all the admonitions. The 
            attributes of this block control the borders and prohibit the admonition
            from breaking across a page.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='default-admonition-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">block</xsl:with-param> 
            <xsl:with-param name="docutils">None</xsl:with-param> 
        </xsl:call-template>
        <block>
            Sets up the defaults for the title blocks of all the admonitions. The 
            attributes of this block control the color (red) and font size. For certain
            blocs, the color is set to black (see below).
        </block>
    </xsl:template>



    <xsl:template match= "xsl:attribute-set[@name='attention-block']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">5</xsl:with-param>
            <xsl:with-param name="text">admonitions outer block</xsl:with-param>
        </xsl:call-template>
        <block>
            <xsl:text>:fo: fo:block</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: attention | caution | danger | error | hint | important | note | tip | warning | admonitons[@classes='custorm']</xsl:text> 
        </block>
     
        
        <block first-line-indent="-9">
            <xsl:text>:inherits: default-admonition-outer-block</xsl:text> 
        </block>
        <block>
            The following attribute sets are identical in nature:
        </block>
        <block>* attention-block</block>
        <block>* caution-block</block>
        <block>* danger-block</block>
        <block>* error-block</block>
        <block>* hint-block</block>
        <block>* important-block</block>
        <block>* note-block</block>
        <block>* tip-block</block>
        <block>* warning-block</block>
        <block>* admonition-custom-block</block>
        <block>
            These attribute-sets format the outer block of all the admonitions. By default it puts 
            an border around the text. Use this attribute set to set the space before or after, the background
            color, etc.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='caution-block']|
                          xsl:attribute-set[@name='danger-block']|
                          xsl:attribute-set[@name='error-block']|
                          xsl:attribute-set[@name='hint-block']|
                          xsl:attribute-set[@name='important-block']|
                          xsl:attribute-set[@name='note-block']|
                          xsl:attribute-set[@name='tip-block']|
                          xsl:attribute-set[@name='warning-block']|
                          xsl:attribute-set[@name='admonition-custom-block']
        " 
        priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='attention-title-block']" priority="3">
        <xsl:call-template name="make-title">
            <xsl:with-param name="level">5</xsl:with-param>
            <xsl:with-param name="text">admonitions title block</xsl:with-param>
        </xsl:call-template>
        <block>
            <xsl:text>:fo: fo:block</xsl:text> 
        </block>
        <block first-line-indent="-9">
            <xsl:text>:docutils: attention | caution | danger | error | hint | important | note | tip | warning | admonitons[@classes='custorm']</xsl:text> 
        </block>
     
        
        <block first-line-indent="-9">
            <xsl:text>:inherits: default-admonition-title-block</xsl:text> 
        </block>
        <block>
            The following attribute sets are identical in nature:
        </block>
        <block>* attention-title-block</block>
        <block>* caution-title-block</block>
        <block>* danger-title-block</block>
        <block>* error-title-block</block>
        <block>* hint-title-block</block>
        <block>* important-title-block</block>
        <block>* note-title-block</block>
        <block>* tip-title-block</block>
        <block>* warning-title-block</block>
        <block>* admonition-custom-title-block</block>
        <block>
            These attribute-sets format the title block of all the admonitions. It sets the
            color to red.
        </block>
        <block>
            The attribute-sets ``error-title-block``, ``hint-title-block``, ``important-title-block``,
            ``note-title-block``, ``tip-title-block``, and ``admonition-custom-title-block`` resets
            the color back to black.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='caution-title-block']|
                          xsl:attribute-set[@name='danger-title-block']|
                          xsl:attribute-set[@name='error-title-block']|
                          xsl:attribute-set[@name='hint-title-block']|
                          xsl:attribute-set[@name='important-title-block']|
                          xsl:attribute-set[@name='note-title-block']|
                          xsl:attribute-set[@name='tip-title-block']|
                          xsl:attribute-set[@name='warning-title-block']|
                          xsl:attribute-set[@name='admonition-custorm-title-block']
        " 
        priority="3"/>

    <xsl:template match= "xsl:attribute-set[@name='admonition-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">attention/paragraph|caution/paragraph|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs in the admonitions. A different attribute-set formats the first
            paragraph (see below).
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='admonition-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">attention/paragraph|caution/paragraph|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs in the admonitions. A different attribute-set formats the first
            paragraph (see below).
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='admonition-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">attention/paragraph[1]|caution/paragraph[1]|etc.</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs in the admonitions. It inherits its attributes from the 
            ``admonition-paragraph-block`` and resets the ``space-before`` property to ``0pt``. It does not 
            make sense to modify the attributes in this set directly.
        </block>
    </xsl:template>


</xsl:stylesheet>
