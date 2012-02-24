<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:d="http://docbook.org/ns/docbook"
    version="1.1"
    >
    <!-- $Id: error.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->
    <xsl:template name="test-params">
    </xsl:template>


    <xsl:template name="trace-ancestors">
        <xsl:param name="children"/>
        <xsl:choose>
            <xsl:when test="parent::*">
                <xsl:for-each select="parent::*">
                    <xsl:call-template name="trace-ancestors">
                        <xsl:with-param name="children">
                            <xsl:value-of select="name(.)"/>
                            <xsl:text>/</xsl:text>
                            <xsl:value-of select="$children"/>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>/</xsl:text>
                <xsl:value-of select="$children"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="trace-siblings">
        <xsl:param name="previous-siblings"/>
        <xsl:choose>
            <xsl:when test="preceding-sibling::*">
                <xsl:for-each select="preceding-sibling::*[1]">
                    <xsl:call-template name="trace-siblings">
                        <xsl:with-param name="previous-siblings">
                            <xsl:value-of select="name(.)"/>
                            <xsl:text>=></xsl:text>
                            <xsl:value-of select="$previous-siblings"/>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>siblings: </xsl:text>
                <xsl:value-of select="$previous-siblings"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template name="trace">
        <xsl:variable name="ancestors">
            <xsl:call-template name="trace-ancestors">
                <xsl:with-param name="children" select="name(.)"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:variable name="siblings">
            <xsl:call-template name="trace-siblings"/>
        </xsl:variable>
        <xsl:value-of select="$ancestors"/>
        <xsl:if test="@*">
            <xsl:text>[</xsl:text>
            <xsl:for-each select="@*">
                <xsl:text>@</xsl:text>
                <xsl:value-of select="name(.)"/>
                <xsl:text>="</xsl:text>
                <xsl:value-of select="."/>
                <xsl:text>" </xsl:text>
            </xsl:for-each>
            <xsl:text>]</xsl:text>
        </xsl:if>
        <xsl:text>&#xA;</xsl:text>
        <xsl:value-of select="$siblings"/>
    </xsl:template>


    <xsl:template match="*">
        <xsl:variable name="trace">
            <xsl:call-template name="trace"/>
        </xsl:variable>
        <xsl:variable name="msg">
            <xsl:text>no match for </xsl:text>
            <xsl:value-of select="$trace"/>
        </xsl:variable>
        <xsl:call-template name="error-message">
            <xsl:with-param name="msg" select="$msg"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="system_message[@type='ERROR']">
        <xsl:message>
            <xsl:text>Error when converting to XML:&#xA;</xsl:text>
            <xsl:value-of select="."/>
        </xsl:message>
        <xsl:if test="$strict='True'">
            <xsl:call-template name="quit-message"/>
        </xsl:if>
    </xsl:template>

    <xsl:template match="system_message[@type='ERROR']/paragraph| system_message[@type='ERROR']/literal_block" priority="2"/>

    <xsl:template match="problematic|system_message|system_message/paragraph">
        <d:para>
            <xsl:apply-templates/>
        </d:para>
    </xsl:template>

    <xsl:template match="split-space">
        <xsl:param name="string"/>
        <xsl:if test="$string != ''">
            
        </xsl:if>
    </xsl:template>

    <xsl:template match="raw[@format]">
        <xsl:if test="@format != 'xml'">
            <xsl:variable name="msg">
                <xsl:text>Raw "</xsl:text>
                <xsl:value-of select="@format"/>
                <xsl:text>" not allowd in an XML document</xsl:text>
            </xsl:variable>
            <xsl:call-template name="error-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>


    <xsl:template name="quit-message">
        <xsl:param name="msg"/>
        <xsl:message terminate="yes">
            <xsl:value-of select="$msg"/>
            <xsl:text>Processing stylesheets now quitting.</xsl:text>
        </xsl:message>
    </xsl:template>

    <!--for unmatched templates-->
    <xsl:template name="error-message">
        <xsl:param name="msg"/>
        <xsl:message>
            <xsl:value-of select="$msg"/>
        </xsl:message>
        <xsl:choose>
            <xsl:when test="normalize-space($strict) != ''">
                <xsl:call-template name="quit-message"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Not processing text for this element.</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="error-message-generic">
        <xsl:param name="quit"/>
        <xsl:param name="msg"/>
        <xsl:message>
            <xsl:value-of select="$msg"/>
        </xsl:message>
        <xsl:if test="normalize-space($quit) != ''">
            <xsl:call-template name="quit-message"/>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>
