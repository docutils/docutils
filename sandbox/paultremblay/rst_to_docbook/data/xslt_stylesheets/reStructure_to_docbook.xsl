<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <!--

    This template changed a XML document that has already been processed by docutils
    with the filed names template to docbook.

    -->


    <xsl:output method='xml' doctype-system='/home/paul/Documents/data/dtds/docbookx.dtd'/>


    <xsl:template match = "document">
        <article>
            <articleinfo>   
                <xsl:apply-templates select = "/document/docinfo/author"/>
                <xsl:choose>
                    <xsl:when test='/document/title'>
                        <xsl:apply-templates select = '/document/title' mode = 'keep'/>
                    </xsl:when>
                    <xsl:when test = '/document/docinfo/title'>
                        <xsl:apply-templates select = '/document/docinfo/title'/>
                    </xsl:when>
                </xsl:choose>


                <xsl:if test = "/document/docinfo/revision">
                    <revhistory>
                        <xsl:apply-templates select = "/document/docinfo/revision"/>
                    </revhistory>
                </xsl:if>


            </articleinfo>
                <xsl:apply-templates/>
        </article>
    </xsl:template>


    <xsl:template match = "/document/docinfo/author">
        <author>
            <xsl:apply-templates/>
        </author>
    </xsl:template>

    <xsl:template match = "/document/docinfo/author/firstname|/document/docinfo/author/first">
        <firstname>
            <xsl:apply-templates/>
        </firstname>
    </xsl:template>


    <xsl:template match = "/document/docinfo/author/surname">
        <surname>
            <xsl:apply-templates/>
        </surname>
    </xsl:template>

    <xsl:template match = "/document/docinfo/author/address|/document/docinfo/author/email">
        <email>
            <xsl:apply-templates/>
        </email>
    </xsl:template>

    <xsl:template match = "/document/docinfo/author/firstname/paragraph|/document/docinfo/author/surname/paragraph|document/docinfo/author/address/paragraph|/document/docinfo/author/first/paragraph|/document/docinfo/author/email/paragraph|/document/docinfo/author/address/pararaph">
        <xsl:apply-templates/>
    </xsl:template>


    <xsl:template match = "/document/docinfo/author/email/paragraph/reference|/document/docinfo/author/address/reference">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match = "/document/docinfo/revision">
        <revision>
            <xsl:apply-templates/>
        </revision>
    </xsl:template>

    <xsl:template match = "revremark/paragraph">
        <xsl:apply-templates/>
    </xsl:template>



    <!--info to delte-->
    <xsl:template match = "docinfo"/>
    <xsl:template match = "/document/title"/>
    <xsl:template match = "system_message"/>
    <xsl:template match = "literal"/>
    <xsl:template match = "substitution_definition"/>
    <xsl:template match = "transition"/>
    <xsl:template match = "substitution_definition"/>
    <xsl:template match = "system_warning"/>


    <!--Info to get rid of paragraph tags-->
    <xsl:template match = "p/paragraph|foreName/paragraph|surname/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match = "/document/docinfo/author">
        <author>
            <xsl:apply-templates/>
        </author>
    </xsl:template>

    <xsl:template match = "/document/docinfo/revnumber">
        <revnumber>
            <xsl:apply-templates/>
        </revnumber>
    </xsl:template>

    <xsl:template match = "/document/docinfo/revision/revnumber/paragraph">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match = "/document/docinfo/revision/date">
        <date>
            <xsl:apply-templates/>
        </date>
    </xsl:template>


    <xsl:template match = "document/docinfo/revision/date/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match = "/document/docinfo/revision/date_bogus">
        <xsl:call-template name = 'my-date'/>
        <!--
        <date>
            <xsl:apply-templates/>
        </date>
        -->
        <respStmt>
            <resp>written by</resp>
            <name>
                Paul Tremblay
            </name>
        </respStmt>
    </xsl:template>
    <xsl:template match = "/document/docinfo/item">
        <item>
            <xsl:apply-templates/>
        </item>
    </xsl:template>

    <xsl:template match = "paragraph">
        <xsl:choose>
            <xsl:when test = "preceding-sibling::*[1]/self::transition">
                
                <!--
            <xsl:when test = "name(preceding-sibling::*[1])='transition'">
                -->
                <para role="transition">
                    <xsl:apply-templates/>
                </para>
            </xsl:when>
            <xsl:otherwise>
                <para>
                    <xsl:apply-templates/>
                </para>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match = 'l/paragraph'>
        <xsl:apply-templates/>
    </xsl:template>

    <!--examples-->

    <xsl:template match = "example[@program]">
        <example>
            <xsl:apply-templates/>
        </example>
    </xsl:template>
    <xsl:template match = "example/title/paragraph">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match = "example[@program]/literal_block">
        <programlisting>
            <xsl:apply-templates/>
        </programlisting>
    </xsl:template>

    <xsl:template match = "literal_block">
        <literallayout>
            <xsl:apply-templates/>
        </literallayout>
    </xsl:template>


    <xsl:template match = "/document/docinfo/title|/document/title" mode = 'keep'>
        <title>
            <xsl:apply-templates/>
        </title>
    </xsl:template>

    <xsl:template match = "section">
        <xsl:element name = "sect1">
            <xsl:attribute name = 'id'>
                <xsl:value-of select = '@id'/>
            </xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>


    <xsl:template match = "section/section">
        <xsl:element name = "sect2">
            <xsl:attribute name = 'id'>
                <xsl:value-of select = '@id'/>
            </xsl:attribute>
            <xsl:attribute name = "type">
                <xsl:value-of select = "@name"/>
            </xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match = "section/section/section">
        <xsl:element name = "sect3">
            <xsl:attribute name = 'id'>
                <xsl:value-of select = '@id'/>
            </xsl:attribute>
            <xsl:attribute name = "type">
                <xsl:value-of select = "@name"/>
            </xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match = "section/section/section/section">
        <xsl:element name = "sect4">
            <xsl:attribute name = 'id'>
                <xsl:value-of select = '@id'/>
            </xsl:attribute>
            <xsl:attribute name = "type">
                <xsl:value-of select = "@name"/>
            </xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match = "section/section/section/section/section">
        <xsl:element name = "sect5">
            <xsl:attribute name = 'id'>
                <xsl:value-of select = '@id'/>
            </xsl:attribute>
            <xsl:attribute name = "type">
                <xsl:value-of select = "@name"/>
            </xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match="section/title">
        <title>
            <xsl:apply-templates/>
        </title>
    </xsl:template>

    <xsl:template match = 'reference'>
        <xsl:element name = "ref">
            <xsl:attribute name = "target">
                <xsl:value-of select = "@refid"/>
            </xsl:attribute>
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>

    <xsl:template match = 'target'>
        <xsl:element name= "anchor">
            <xsl:attribute name = "id">
                <xsl:value-of select = "@id"/>
            </xsl:attribute>
        </xsl:element>
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match = "instant">
        <dateline>
            <date>
                <xsl:value-of select = "substring(.,0,11)"/>
            </date>
            <xsl:element name = "time">
                <xsl:attribute name = "value">
                    <xsl:value-of select = "substring(.,12,8)"/>
                </xsl:attribute>
                <xsl:attribute name = 'zone'>
                    <xsl:choose>
                        <xsl:when test = 'substring(normalize-space(.),20,6)'>
                            <xsl:value-of select = 'substring(normalize-space(.),20,6)'/>
                        </xsl:when>
                        <xsl:otherwise>
                            <xsl:text>-05:00</xsl:text>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:attribute>
                <xsl:attribute name = "type">
                    <xsl:text>24hour</xsl:text>
                </xsl:attribute>
            </xsl:element>
        </dateline>
    </xsl:template>

    <xsl:template name = "my-date">
        <xsl:element name = 'date'>
            <xsl:attribute name = 'value'>
                <xsl:value-of select = "substring(normalize-space(.),0,11)"/>
            </xsl:attribute>
        </xsl:element>
    </xsl:template>

    <!--This is just and example of how to parse time?-->
    <xsl:template name = "my-time">
        <test>
            <xsl:value-of select='normalize-space(.)'/>
        </test>
        <xsl:element name = 'date'>
            <xsl:attribute name = 'value'>
                <xsl:value-of select = "substring(normalize-space(.),0,11)"/>
            </xsl:attribute>
            <xsl:attribute name = 'zone'>
                <xsl:choose>
                    <xsl:when test = 'substring(normalize-space(.),12,13)'>
                        <xsl:value-of select = 'substring(normalize-space(.),12,5)'/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text>-05:00</xsl:text>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:attribute>
            <xsl:attribute name = "type">
                <xsl:text>24hour</xsl:text>
            </xsl:attribute>
        </xsl:element>
    </xsl:template>
    <xsl:template match = 'body'>
        <text>
            <body>
                <xsl:apply-templates/>
            </body>
        </text>
    </xsl:template>

    <xsl:template match = 'block_quote'>
        <q rend = "block">
            <xsl:apply-templates/>
        </q>
    </xsl:template>

    <xsl:template match = "comment">
        <xsl:comment>
            <xsl:value-of select = "."/>
        </xsl:comment>
    </xsl:template>

    <xsl:template match = "emphasis">
        <emph>
            <xsl:apply-templates/>
        </emph>
    </xsl:template>

    <xsl:template match = "enumerated_list">
        <list type="ordered">
            <xsl:apply-templates/>
        </list>
    </xsl:template>

    <xsl:template match = "list_item">
        <item>
            <xsl:apply-templates/>
        </item>
    </xsl:template>

    
    <xsl:template match="@*|node()">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
