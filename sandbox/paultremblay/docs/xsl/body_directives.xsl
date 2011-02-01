<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <!--topic-->
    <xsl:template match= "xsl:attribute-set[@name='topic-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the outermost block of the topic element, which contains blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='topic-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic/title</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title of the topic.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='topic-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the topic. A different set of attributes 
            formats the first paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='topic-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">topic/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">topic-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs of the topic. 
        </block>
    </xsl:template>

    <!--sidebar-->

    <xsl:template match= "xsl:attribute-set[@name='sidebar-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">sidebar</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the outermost block of the sidebar element, which contains blocks.
            Note that fop does not handle floats, so this element is formatted
            just like a topic block.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='sidebar-title-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">sidebar/title</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the title of the topic.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='sidebar-subtitle-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">sidebar/subtitle</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the subtitle of the topic.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='sidebar-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">sidebar/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the sidebar. A different set of attributes 
            formats the first paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='sidebar-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">sidebar/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">sidebar-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs of the sidebar. 
        </block>
    </xsl:template>


    <!--rubric-->
    <xsl:template match= "xsl:attribute-set[@name='rubric-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">rubric</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the rubric.
        </block>
    </xsl:template>

    <!--epigraph-->

    <xsl:template match= "xsl:attribute-set[@name='epigraph-outer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">epigraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the outermost block of the epigraph element, which contains blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='epigraph-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">epigraph/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the epigraph. A different set of attributes 
            formats the first paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='epigraph-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">epigraph/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">epigraph-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs of the epigraph. 
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='epigraph-attribution-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">epigraph/attribution</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the attribution of the epigraph. The parameter 
            ``text-before-epigraph-attribution`` determines the text to 
            put before the attribtion. The default is '&#x2014;' (an em-dash). To
            put no text before, set this parameter to an empty string.
        </block>
    </xsl:template>

    <!--highlights-->
    <xsl:template match= "xsl:attribute-set[@name='highlights-outer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">highlights</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the outermost block of the epigraph element, which contains blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='highlights-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">highlights/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the highlights. A different set of attributes 
            formats the first paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='highlights-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">highlights/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">highlights-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs of the highlights. 
        </block>
    </xsl:template>

    <!--pull-quote-->

    <xsl:template match= "xsl:attribute-set[@name='pull-quote-outer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">pull-quote</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the outermost block of the pull-quote element, which contains blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='pull-quote-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">pull-quote/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the pull-quote. A different set of attributes 
            formats the first paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='pull-quote-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">pull-quote/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">pull-quote-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs of the pull-quote. 
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='pull-quote-attribution-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">pull-quote/attribution</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the attribution of the pull-quote. The parameter 
            ``text-before-pull-quote-attribution`` determines the text to 
            put before the attribtion. The default is '&#x2014;' (an em-dash). To
            put no text before, set this parameter to an empty string.
        </block>
    </xsl:template>

    <!--container-->
    <xsl:template match= "xsl:attribute-set[@name='container-outer-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the outermost block of the container element, which contains blocks.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='container-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container/paragraph</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the paragraphs of the container. A different set of attributes 
            formats the first paragraph.
        </block>
    </xsl:template>

    <xsl:template match= "xsl:attribute-set[@name='container-first-paragraph-block']" priority="3">
        <xsl:call-template name="before-desc">
            <xsl:with-param name="fo">fo:block</xsl:with-param> 
            <xsl:with-param name="docutils">container/paragraph[1]</xsl:with-param> 
            <xsl:with-param name="inherits">container-paragraph-block</xsl:with-param> 
        </xsl:call-template>
        <block>
            Formats the first paragraphs of the container. 
        </block>
    </xsl:template>


</xsl:stylesheet>
