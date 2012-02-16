<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.1"
>

    <xsl:output method="xml" encoding="UTF-8"/>


    <xsl:template match="@*|node()">
        <xsl:copy> 
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>


    <xsl:template match="comment()">
        <xsl:comment>
            <xsl:value-of select="."/>
        </xsl:comment>
    </xsl:template>

    <xsl:template match="table">
        <xsl:copy-of select="."/>
        <xsl:apply-templates select="following-sibling::container[1][@classes='caption']" mode="keep"/>
    </xsl:template>

    <xsl:template match="container[@classes='caption']" mode="keep">
        <xsl:copy-of select="."/>
    </xsl:template>
    <xsl:template match="container[@classes='caption']" />

    <xsl:template name="split-table">
        <xsl:param name="first-page-rows"/>
        <xsl:variable name="num-rows" select="count(descendant::row)"/>
        <xsl:if test="$first-page-rows &gt;= $num-rows">
            <xsl:message terminate="yes">
                <xsl:text>Cannot split tables because first-page-rows "</xsl:text>
                <xsl:value-of select="$first-page-rows"/>
                <xsl:text>" is greater than or equal to number of rows "</xsl:text>
                <xsl:value-of select="$num-rows"/>
                <xsl:text>" &#xA;Script now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:copy >
            <xsl:copy-of select="@*"/>
            <xsl:copy-of select="title"/>
            <tgroup>
                <xsl:copy-of select="tgroup/@*"/>
                <xsl:copy-of select="tgroup/colspec"/>
                <xsl:copy-of select="tgroup/thead"/>
                <xsl:copy-of select="tgroup/tfoot"/>
                <tbody>
                    <xsl:copy-of select="tgroup/tbody/@*"/>
                    <xsl:copy-of select="tgroup/tbody/row[position() &lt;=
                        $first-page-rows]"/>
                </tbody>
            </tgroup>
        </xsl:copy>
        <table >
            <xsl:copy-of select="@*"/>
            <tgroup>
                <xsl:copy-of select="tgroup/@*"/>
                <xsl:copy-of select="tgroup/colspec"/>
                <thead>
                    <xsl:copy-of select="tgroup/thead/@*"/>
                    <row classes="continuation-label">
                        <entry>
                            <xsl:attribute name="morecols">
                                <xsl:value-of select="tgroup/@cols"/>
                            </xsl:attribute>
                            <paragraph>
                                <xsl:apply-templates select="title" mode="continue-label"/>
                            </paragraph>
                        </entry>
                    </row>
                    <xsl:apply-templates select="tgroup/thead/*"/>
                </thead>
                <xsl:copy-of select="tgroup/tfoot"/>
                <tbody>
                    <xsl:copy-of select="tgroup/tbody/@*"/>
                    <xsl:copy-of select="tgroup/tbody/row[position() &gt;
                        $first-page-rows]"/>
                </tbody>
            </tgroup>
        </table>
        <xsl:apply-templates select="following-sibling::container[1][@classes='caption']" mode="keep"/>
    </xsl:template>

    <xsl:template match="title" mode="continue-label">
        <!--do nothing here, because the fo templates overwrite the content
             here
        <xsl:apply-templates/>
        -->
    </xsl:template>

</xsl:stylesheet>

