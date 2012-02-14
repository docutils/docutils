<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id: util.xsl 7131 2011-09-26 19:27:15Z paultremblay $ -->


    <xsl:variable name="toc-exists">
        <xsl:if test="/document/topic[@classes='contents']">True</xsl:if>
    </xsl:variable>
    <xsl:variable name="dedication-exists">
        <xsl:if test="/document/topic[@classes='dedication']">True</xsl:if>
    </xsl:variable>
    <xsl:variable name="abstract-exists">
        <xsl:if test="/document/topic[@classes='abstract']">True</xsl:if>
    </xsl:variable>
    <xsl:variable name="title-exists">
        <xsl:if test="/document/title">True</xsl:if>
    </xsl:variable>
    <xsl:variable name="bibliographic-exists">
        <xsl:if test="/document/docinfo">True</xsl:if>
    </xsl:variable>
    <!--determine page sequence, to be used in other templates-->
    <xsl:variable name="page-sequence-type">
        <xsl:variable name="need-front-matter-page-sequence">
            <xsl:choose>
                <xsl:when test="$title-exists='True' and $title-pagination='with-front'">True</xsl:when>
                <xsl:when test="$bibliographic-exists='True' and $bibliographic-pagination='with-front'">True</xsl:when>
                <xsl:when test="$abstract-exists='True' and $abstract-pagination='with-front'">True</xsl:when>
                <xsl:when test="$dedication-exists='True' and $dedication-pagination='with-front'">True</xsl:when>
                <xsl:when test="$toc-exists='True' and $toc-pagination='with-front'">True</xsl:when>
                <xsl:otherwise>False</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="need-toc-page-sequence">
            <xsl:choose>
                <xsl:when test="$title-exists='True' and $title-pagination='with-toc'">True</xsl:when>
                <xsl:when test="$bibliographic-exists='True' and $bibliographic-pagination='with-toc'">True</xsl:when>
                <xsl:when test="$abstract-exists='True' and $abstract-pagination='with-toc'">True</xsl:when>
                <xsl:when test="$dedication-exists='True' and $dedication-pagination='with-toc'">True</xsl:when>
                <xsl:when test="$toc-exists='True' and $toc-pagination='with-toc'">True</xsl:when>
                <xsl:otherwise>False</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$need-toc-page-sequence = 'True' and $need-front-matter-page-sequence = 'True'">
                <xsl:text>front-toc-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-front-matter-page-sequence = 'True'">
                <xsl:text>front-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-toc-page-sequence = 'True'">
                <xsl:text>toc-body</xsl:text>
            </xsl:when>
            <xsl:when test="$need-toc-page-sequence = 'False' and $need-front-matter-page-sequence = 'False'">
                <xsl:text>body</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate = "yes">
                    <xsl:text>Stylsheet error: no page sequence found</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>

    <xsl:variable name="layout-page">
        <xsl:choose>
            <xsl:when test = "$page-layout != ''">
                <xsl:value-of select="$page-layout"/>
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'body-header']|/document/container[@classes='body-footer']">
                <xsl:text>first</xsl:text> 
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'toc-body-header']|/document/container[@classes='toc-body-footer']">
                <xsl:text>first</xsl:text> 
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'first-header']|/document/container[@classes='first-footer']">
                <xsl:choose>
                    <xsl:when test="/document/container[@classes = 'odd-header']|/document/container[@classes = 'odd-footer']">
                        <xsl:text>first-odd-even</xsl:text> 
                    </xsl:when>
                    <xsl:when test="/document/container[@classes = 'even-header']|/document/container[@classes = 'even-footer']">
                        <xsl:text>first-odd-even</xsl:text> 
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text>first</xsl:text>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'toc-first-header']|/document/container[@classes='toc-first-footer']">
                <xsl:choose>
                    <xsl:when test="/document/container[@classes = 'toc-odd-header']|/document/container[@classes = 'toc-odd-footer']">
                        <xsl:text>first-odd-even</xsl:text> 
                    </xsl:when>
                    <xsl:when test="/document/container[@classes = 'toc-even-header']|/document/container[@classes = 'toc-even-footer']">
                        <xsl:text>first-odd-even</xsl:text> 
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:text>first</xsl:text>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'odd-header']|/document/container[@classes='odd-footer']">
                <xsl:text>odd-even</xsl:text>
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'toc-odd-header']|/document/container[@classes='toc-odd-footer']">
                <xsl:text>odd-even</xsl:text>
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'even-header']|/document/container[@classes='even-footer']">
                <xsl:text>odd-even</xsl:text>
            </xsl:when>
            <xsl:when test = "/document/container[@classes = 'toc-even-header']|/document/container[@classes='toc-even-footer']">
                <xsl:text>odd-even</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>simple</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:variable>

    <xsl:template name="my-boolean-func">
        <xsl:param name="string"/>
        <xsl:param name="name"/>
        <xsl:param name="allow-empty-string" select="boolean('true')"/>
        <xsl:choose>
            <xsl:when test="$string = '' and $allow-empty-string">
                <xsl:value-of select= "boolean('')"/>
            </xsl:when>
            <xsl:when test="$string = 'yes' or $string = 'Yes' or $string = 'true' or $string = 'True'">
                <xsl:value-of select= "boolean('true')"/>
            </xsl:when>
            <xsl:when test="$string = 'no' or $string = 'No' or $string = 'false' or $string = 'False'">
                <xsl:value-of select= "boolean('')"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:variable name="msg">
                    <xsl:text>"</xsl:text>
                    <xsl:value-of select="$string"/>
                    <xsl:text>"</xsl:text>
                    <xsl:text> not a valid boolean value for </xsl:text>
                    <xsl:value-of select="$name"/>
                    <xsl:text>&#xA;</xsl:text>
                </xsl:variable>
                <xsl:call-template name="quit-message">
                    <xsl:with-param name="msg" select="$msg"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:variable name="custom-spc-header-footer">
        <xsl:call-template name="my-boolean-func">
            <xsl:with-param name="string" select="$custom-spacing-header-footer"/>
            <xsl:with-param name="name" select="'custom-spacing-header-footer'"/>
        </xsl:call-template>
    </xsl:variable>

    
</xsl:stylesheet> 
