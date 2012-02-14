<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="table">
        <d:table xsl:use-attribute-sets="table" rowsep="0" colsep="0">
            <xsl:if test="following-sibling::container[1][@classes = 'caption']/paragraph/target">
                <xsl:attribute name="xml:id">
                    <xsl:value-of select="following-sibling::container[1][@classes = 'caption']/paragraph/target/@ids"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="not(title)">
                <d:title>
                    <xsl:value-of select="@classes"/>
                </d:title>
            </xsl:if>
            <xsl:apply-templates/>
            <xsl:apply-templates select="following-sibling::container[1][@classes='caption']" mode="table"/>
        </d:table>
    </xsl:template>

    <xsl:template match="table/title">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template>

    <xsl:template match="tgroup">
        <d:tgroup cols="{@cols}">
            <xsl:apply-templates/>
        </d:tgroup>
    </xsl:template>


    <xsl:template match="row">
        <d:row>
            <xsl:apply-templates/>
        </d:row>
    </xsl:template>

    <xsl:template match="entry">
        <d:entry>
            <xsl:apply-templates/>
        </d:entry>
    </xsl:template>

    <xsl:template match="colspec">
        <xsl:variable name="class" select="ancestor::table/@classes"/>
        <xsl:variable name="position">
            <xsl:number/>
        </xsl:variable>
        <d:colspec>
            <xsl:attribute name="colwidth">
                <xsl:value-of select="@colwidth"/>
                <xsl:text>*</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="align">
                <xsl:text >left</xsl:text>
            </xsl:attribute>
        </d:colspec>
    </xsl:template>


    <xsl:template match="entry/paragraph">
        <d:para>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>

    <xsl:template match="thead">
        <d:thead>
            <xsl:apply-templates/>
        </d:thead>
    </xsl:template>

    <xsl:template match="tbody">
        <d:tbody>
            <xsl:apply-templates/>
        </d:tbody>
    </xsl:template>

    <xsl:template match="container[@classes='caption']" mode="table">
        <d:caption>
            <xsl:apply-templates/>
        </d:caption>
    </xsl:template>

    <xsl:template match="container[@classes='caption']/paragraph">
        <d:para>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>
    
</xsl:stylesheet>
