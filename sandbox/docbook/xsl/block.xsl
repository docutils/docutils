<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:key name="footnote" match="footnote" use="@ids"/>
    <xsl:template
        match="document/paragraph|section/paragraph|block_quote/paragraph|
        attention/paragraph|caution/paragraph|admonition/paragraph|
        danger/paragraph|hint/paragraph|important/paragraph|
        note/paragraph|tip/paragraph|warning/paragraph|
        docinfo/field/field_body/paragraph|/document/topic[@classes='dedication']/paragraph|
        /document/topic[@classes='abstract']/paragraph|
        list_item/paragraph|container/paragraph|legend/paragraph|
        footnote/paragraph|description/paragraph|sidebar/paragraph">
        <d:para>
            <xsl:call-template name="make-roles"/>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>

    <xsl:template match="section/block_quote|block_quote">
        <d:blockquote>
            <xsl:call-template name="make-roles"/>
            <xsl:apply-templates select="attribution" mode="with-block-quote"/>
            <xsl:apply-templates/>
        </d:blockquote>
    </xsl:template>

    <xsl:template match="attribution" mode="with-block-quote">
        <d:attribution >
            <xsl:apply-templates/>
        </d:attribution>
    </xsl:template>

    <xsl:template match="error/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="block_quote/attribution"/>

    <xsl:template match="compound_">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="compound">
        <d:para role="compound">
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>

    <xsl:template match="compound/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <!--use programlisting?? for now-->
    <xsl:template match="literal_block_">
        <d:literallayout xml:space="preserve">
            <xsl:apply-templates/>
        </d:literallayout>
    </xsl:template>

    <xsl:template match="container[@classes='caption']"/>
    <!--imperfect match for container-->
    <xsl:template match="container[not(@classes='caption')]">
        <d:section role="{@classes}">
            <d:title>
                <xsl:value-of select="@classes"/>
            </d:title>
            <xsl:apply-templates/>
        </d:section>
    </xsl:template>

    <xsl:template match="epigraph">
        <d:epigraph>
            <xsl:apply-templates/>
        </d:epigraph>
    </xsl:template>

    <xsl:template match="footnote" mode="footnote">
        <d:footnote>
            <xsl:if test= "string(number(label))='NaN'">
                <xsl:attribute name="label">
                    <xsl:value-of select="label"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates/>
        </d:footnote>
    </xsl:template>

    <xsl:template match="footnote_reference">
        <!--need to check that there are no ancestors in a footnote reference, or 
        you could get infinite recursion-->
        <xsl:if test="not(ancestor::footnote)">
            <xsl:apply-templates select="key('footnote', @refid)" mode="footnote"/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="footnote|footnote/label"/>

    <xsl:template match="rubric">
        <d:bridgehead>
            <xsl:apply-templates/>
        </d:bridgehead>
    </xsl:template>

    <xsl:template match="sidebar">
        <d:sidebar>
            <xsl:apply-templates/>
        </d:sidebar>
    </xsl:template>

    <xsl:template match="sidebar/title">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template>

    <xsl:template match="sidebar/subtitle">
        <xsl:call-template name="error-message">
            <xsl:with-param name="msg">
                <xsl:text >docbook does not allow subtitle for sidebars</xsl:text>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="substitution_definition|comment"/>

    <xsl:template match="processing-instruction('hard-pagebreak')">
        <d:para>
            <xsl:processing-instruction name="hard-pagebreak"/>
        </d:para>
    </xsl:template>
    
</xsl:stylesheet>
