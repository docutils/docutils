<!--WARNING TO PAUL: DON'T EDIT THIS FILE use update.py instead.
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <!--

    This template changes field_name attributes to tags and attributes:


    -->




    <xsl:template match = "field">
        <xsl:element name = "{field_name/arguments[1]}">
            <xsl:if test= "field_name/arguments[2]">
                <xsl:attribute name = "{field_name/arguments[2]}">
                    <xsl:value-of select = "field_name/arguments[3]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[4]">
                <xsl:attribute name = "{field_name/arguments[4]}">
                    <xsl:value-of select = "field_name/arguments[5]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[6]">
                <xsl:attribute name = "{field_name/arguments[6]}">
                    <xsl:value-of select = "field_name/arguments[7]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[8]">
                <xsl:attribute name = "{field_name/arguments[8]}">
                    <xsl:value-of select = "field_name/arguments[9]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[10]">
                <xsl:attribute name = "{field_name/arguments[10]}">
                    <xsl:value-of select = "field_name/arguments[11]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[12]">
                <xsl:attribute name = "{field_name/arguments[12]}">
                    <xsl:value-of select = "field_name/arguments[13]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[14]">
                <xsl:attribute name = "{field_name/arguments[14]}">
                    <xsl:value-of select = "field_name/arguments[15]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[16]">
                <xsl:attribute name = "{field_name/arguments[16]}">
                    <xsl:value-of select = "field_name/arguments[17]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[18]">
                <xsl:attribute name = "{field_name/arguments[18]}">
                    <xsl:value-of select = "field_name/arguments[19]"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test= "field_name/arguments[20]">
                <xsl:attribute name = "{field_name/arguments[20]}">
                    <xsl:value-of select = "field_name/arguments[21]"/>
                </xsl:attribute>
            </xsl:if>
        <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="inline">
        <xsl:if test="@arg1">
            <xsl:element name = "{@arg1}">
                <xsl:if test = "@arg2">
                    <xsl:attribute name = "{@arg2}">
                        <xsl:value-of select = "@arg3"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test = "@arg4">
                    <xsl:attribute name = "{@arg4}">
                        <xsl:value-of select="@arg5"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test = "@arg6">
                    <xsl:attribute name = "{@arg6}">
                        <xsl:value-of select = "@arg7"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:if test = "@arg8">
                    <xsl:attribute name = "{@arg8}">
                        <xsl:value-of select = "@arg9"/>
                    </xsl:attribute>
                </xsl:if>
                <xsl:apply-templates/>
            </xsl:element>
        </xsl:if>
    </xsl:template>
        

    <xsl:template match = 'field_list'>
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match = "field_name"/>

    <xsl:template match = 'field_body'>
        <xsl:apply-templates/>
    </xsl:template>

    
    <xsl:template match="@*|node()">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
