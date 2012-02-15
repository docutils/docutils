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
        <xsl:call-template name="split-table">
            <xsl:with-param name="first-page-rows" select="2"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="split-table">
        <xsl:param name="first-page-rows"/>
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
    </xsl:template>

    <xsl:template match="title" mode="continue-label">
        <xsl:apply-templates/>
        <xsl:text > (cont.)</xsl:text>
    </xsl:template>

</xsl:stylesheet>

