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

    <xsl:template match="table">
        <d:table xsl:use-attribute-sets="table">
            <xsl:call-template name="make-id"/>
            <xsl:if test="@classes = 'borderless'">
                <xsl:attribute name="rowsep">0</xsl:attribute>
                <xsl:attribute name="colsep">0</xsl:attribute>
            </xsl:if>
            <xsl:if test="@classes">
                <xsl:attribute name="class">
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
            <xsl:if test="not(title)">
                <!-- should be an informal table-->
                <d:title>
                    <xsl:value-of select="@classes"/>
                </d:title>
            </xsl:if>
            <xsl:call-template name="make-table-width">
                <xsl:with-param name="classes" select="@classes"/>
            </xsl:call-template>
            <xsl:apply-templates/>
            <xsl:apply-templates select="following-sibling::container[1][@classes='caption']" mode="table"/>
        </d:table>
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
            <xsl:call-template name="make-table-footer">
                <xsl:with-param name="classes" select="../@classes"/>
            </xsl:call-template>
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
        <xsl:variable name="classes" select="ancestor::table/@classes"/>
        <xsl:variable name="position">
            <xsl:number/>
        </xsl:variable>
        <d:colspec>
            <xsl:attribute name="colwidth">
                <xsl:value-of select="@colwidth"/>
                <xsl:text>*</xsl:text>
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
            <xsl:attribute name="rowsep">
                <xsl:call-template name="head.rowsep">
                    <xsl:with-param name="classes" select="../../@classes"/>
                </xsl:call-template>
            </xsl:attribute>
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

    <xsl:template name="make-table-footer"/>

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

    <xsl:template name="head.rowsep">
        <xsl:text >1</xsl:text>
    </xsl:template>

    <xsl:template name="foot.rowsep">
        <xsl:text >1</xsl:text>
    </xsl:template>

    <xsl:template name="tabstyle">
        <xsl:text >default</xsl:text>
    </xsl:template>
    
</xsl:stylesheet>
