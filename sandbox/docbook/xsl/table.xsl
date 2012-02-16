<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1">

    <xsl:template match="table[@classes='after-abstract']" priority="2">
        <d:informaltable xsl:use-attribute-sets="table">
            <xsl:call-template name="make-id"/>
            <xsl:attribute name="role">after-abstract</xsl:attribute>
            <xsl:apply-templates/>
        </d:informaltable>
    </xsl:template>

    <xsl:template match="table[title]">
        <d:table xsl:use-attribute-sets="table">
            <xsl:call-template name="table-insides"/>
        </d:table>
    </xsl:template>

    <xsl:template match="table[not(title)]">
        <xsl:if test="descendant::row[@classes = 'continuation-label']">
            <d:para>
                <xsl:processing-instruction name="hard-pagebreak"/>
            </d:para>
        </xsl:if>
        <d:informaltable xsl:use-attribute-sets="table">
            <xsl:call-template name="table-insides"/>
        </d:informaltable>
    </xsl:template>

    <xsl:template name="table-insides">
        <xsl:call-template name="make-id"/>
        <xsl:if test="@classes = 'borderless'">
            <xsl:attribute name="rowsep">0</xsl:attribute>
            <xsl:attribute name="colsep">0</xsl:attribute>
        </xsl:if>
        <xsl:if test="@classes">
            <xsl:attribute name="role">
                <xsl:value-of select="@classes"/>
            </xsl:attribute>
        </xsl:if>
        <xsl:attribute name="tabstyle">
            <xsl:call-template name="tabstyle">
                <xsl:with-param name="classes" select="@classes"/>
            </xsl:call-template>
        </xsl:attribute>
        <xsl:attribute name="rowsep">
            <xsl:call-template name="table.rowsep">
                <xsl:with-param name="classes" select="@classes"/>
            </xsl:call-template>
        </xsl:attribute>
        <xsl:attribute name="colsep">
            <xsl:call-template name="table.colsep">
                <xsl:with-param name="classes" select="@classes"/>
            </xsl:call-template>
        </xsl:attribute>
        <xsl:call-template name="make-table-width">
            <xsl:with-param name="classes" select="@classes"/>
        </xsl:call-template>
        <xsl:call-template name="table-keep-together">
            <xsl:with-param name="classes" select="@classes"/>
        </xsl:call-template>
        <xsl:apply-templates/>
        <xsl:for-each select="following-sibling::*[1]">
            <xsl:if test="self::container[@classes = 'caption']">
                <xsl:apply-templates select="self::container" mode="table"/>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>

    <xsl:template match="table/title">
        <d:title>
            <xsl:apply-templates/>
        </d:title>
    </xsl:template>

    <xsl:template match="table/title/target"/>

    <xsl:template match="tgroup">
        <d:tgroup cols="{@cols}">
            <xsl:apply-templates/>
        </d:tgroup>
    </xsl:template>


    <xsl:template match="row">
        <d:row>
            <xsl:if test="@classes">
                <xsl:attribute name="role">
                    <xsl:value-of select="@classes"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates/>
        </d:row>
    </xsl:template>

    <xsl:template match="entry">
        <xsl:variable name="position">
            <xsl:number/>
        </xsl:variable>
        <d:entry>
            <xsl:if test="@morecols">
                <xsl:attribute name="namest">
                    <xsl:text >col</xsl:text>
                    <xsl:value-of select="$position"/>
                </xsl:attribute>
                <xsl:attribute name="nameend">
                    <xsl:text >col</xsl:text>
                    <xsl:value-of select="$position + @morecols - 1"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates/>
        </d:entry>
    </xsl:template>

    <xsl:template match="colspec">
        <xsl:variable name="classes" select="ancestor::table/@classes"/>
        <xsl:variable name="position">
            <xsl:number/>
        </xsl:variable>
        <d:colspec>
            <xsl:attribute name="colwidth">
                <xsl:value-of select="@colwidth"/>
                <xsl:text>*</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="colname">
                <xsl:text>col</xsl:text>
                <xsl:value-of select="$position"/>
            </xsl:attribute>
            <xsl:attribute name="colnum">
                <xsl:value-of select="$position"/>
            </xsl:attribute>
            <xsl:attribute name="align">
                <xsl:call-template name="colspec-align">
                    <xsl:with-param name="position" select="$position"/>
                    <xsl:with-param name="classes" select="$classes"/>
                </xsl:call-template>
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
        <xsl:call-template name="make-table-foot">
            <xsl:with-param name="classes" select="../../@classes"/>
        </xsl:call-template>
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

    <xsl:template name="make-table-foot"/>

    <xsl:template name="colspec-align">
        <xsl:text >left</xsl:text>
    </xsl:template>

    <xsl:template name="make-table-width"/>

    <xsl:template name="table.rowsep">
        <xsl:text >1</xsl:text>
    </xsl:template>

    <xsl:template name="table.colsep" >
        <xsl:text >1</xsl:text>
    </xsl:template>


    <xsl:template name="tabstyle">
        <xsl:text >default</xsl:text>
    </xsl:template>

    <xsl:template name="table-keep-together">
        <xsl:param name="classes"/>
    </xsl:template>
    
</xsl:stylesheet>
