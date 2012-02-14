<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.1"
>
<!--
An example of how to group lines in line blocks in a sensible manner. Note that if the last line of a stanza is indented, 
then the next line will not be blank in RST:

.. incorrect for making the 3rd stanza
| `stanza title 2`
| Half a bee, philosophically,
|     must, *ipso facto*, half not be.
| But half the bee has got to be,
|     *vis a vis* its entity.  D'you see?
|
| stanza title 3 :title:`x` 

An extra line must be inserted:

| `stanza title 2`
| Half a bee, philosophically,
|     must, *ipso facto*, half not be.
| But half the bee has got to be,
|     *vis a vis* its entity.  D'you see?
|
|
| stanza title 3 :title:`x` 

Note that this stylesheet prodcuces invalid XML according to the DTD of RST, since the attributes "type" "n" and "level" are
(probably) not allowed.

-->

    <xsl:output method="xml" encoding="UTF-8"/>


    <xsl:template match="@*|node()">
        <xsl:copy> 
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="line_block/line_block"/>

    <xsl:template match="line_block">
        <xsl:variable name="num-blank-lines">
            <xsl:value-of select="count(descendant::line[normalize-space(.) = ''])"/>
        </xsl:variable>
        <line_block>
            <xsl:choose>
                <xsl:when test="$num-blank-lines = 0">
                    <xsl:for-each select="descendant::line">
                        <xsl:call-template name="process-line">
                            <xsl:with-param name="num-working-on" select="0"/>
                        </xsl:call-template>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="group-lines">
                        <xsl:with-param name="total-blank" select="$num-blank-lines"/>
                    </xsl:call-template>
                </xsl:otherwise>
            </xsl:choose>
        </line_block>
    </xsl:template>

    <xsl:template name="group-lines">
        <xsl:param name="num-working-on">0</xsl:param>
        <xsl:param name="total-blank"/>
        <xsl:if test="$num-working-on &lt;= $total-blank">
            <line_block type="stanza">
                <xsl:for-each select="descendant::line">
                    <xsl:call-template name="process-line">
                        <xsl:with-param name="num-working-on" select="$num-working-on"/>
                    </xsl:call-template>
                </xsl:for-each>
            </line_block>
            <xsl:call-template name="group-lines">
                <xsl:with-param name="total-blank" select="$total-blank"/>
                <xsl:with-param name="num-working-on" select="$num-working-on + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="process-line">
        <xsl:param name="num-working-on"/>
        <xsl:variable name="previous-blanks">
            <xsl:call-template name="get-num-blank-lines"/>
        </xsl:variable>
        <xsl:variable name="level" select="count(ancestor::line_block)"/>
        <xsl:variable name="n">
            <xsl:call-template name="get-line-number"/>
        </xsl:variable>
        <xsl:if test="$num-working-on = $previous-blanks">
            <xsl:choose>
                <xsl:when test="title_reference|inline[@classes='title']">
                    <title>
                        <xsl:apply-templates/>
                    </title>
                </xsl:when>
                <xsl:when test="normalize-space(.) = ''"/>
                <xsl:otherwise>
                    <line n="{$n}" level="{$level}">
                        <xsl:apply-templates/>
                    </line>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:if>
    </xsl:template>


    <xsl:template name="get-num-blank-lines">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) = ''][not(title_reference)][not(inline[@classes='title'])]" level="any"/>
    </xsl:template>


    <xsl:template name="get-line-number">
        <xsl:variable name="top-block-id">
            <xsl:for-each select="ancestor::line_block[last()]">
                <xsl:value-of select="generate-id()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:number from="line_block[generate-id() = $top-block-id]" 
            count="line[normalize-space(.) != ''][not(title_reference)][not(inline[@classes='title'])]" level="any"/>
    </xsl:template>

    <xsl:template match="line/title_reference|inline[@classes='title']">
        <xsl:apply-templates/>
    </xsl:template>


    <xsl:template match="comment()">
        <xsl:comment>
            <xsl:value-of select="."/>
        </xsl:comment>
    </xsl:template>

</xsl:stylesheet>

