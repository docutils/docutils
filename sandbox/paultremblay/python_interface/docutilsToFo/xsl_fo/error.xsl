<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:template name="test-params">

        <xsl:if test= "$title-pagination != 'with-front'  and $title-pagination != 'with-toc' and 
            $title-pagination != 'with-body'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "title-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'with-front', 'with-body', or 'with-body'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>

        <xsl:if test= "$bibliographic-pagination != 'with-front'  and $bibliographic-pagination != 'with-toc' and 
            $bibliographic-pagination != 'with-body'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "bibliographic-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'with-front', 'with-body', or 'with-body'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>

        <xsl:if test= "$dedication-pagination != 'with-front'  and $dedication-pagination != 'with-toc' and 
            $dedication-pagination != 'with-body'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "dedication-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'with-front', 'with-body', or 'with-body'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>

        <xsl:if test= "$abstract-pagination != 'with-front'  and $abstract-pagination != 'with-toc' and 
            $abstract-pagination != 'with-body'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "abstract-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'with-front', 'with-body', or 'with-body'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>

        <xsl:if test= "$toc-pagination != 'with-front'  and $toc-pagination != 'with-toc' and 
            $toc-pagination != 'with-body'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "toc-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'with-front', 'with-body', or 'with-body'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>

        <xsl:if test= "$page-layout != '' and $page-layout != 'simple'  and $page-layout != 'odd-even' 
            and $page-layout != 'first-odd-even' and $page-layout != 'first'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "page-layout"&#xA;</xsl:text>
                <xsl:text>Valid values are 'simple', 'odd-even', 'first', or 'first-odd-even'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test= "$option-list-format != 'list'  and $option-list-format != 'definition'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$option-list-format"/>
                <xsl:text>" not a valid value for param "option-list-format"&#xA;</xsl:text>
                <xsl:text>Valid values are 'list', and 'definition'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test ="$number-verse != '' and string($number-verse + 1 ) = 'NaN'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$number-verse"/>
                <xsl:text>" not a valid value for param "number-verse"&#xA;</xsl:text>
                <xsl:text>Please use a number&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test= "$table-title-placement != 'bottom'  and $table-title-placement != 'top'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$table-title-placement"/>
                <xsl:text>" not a valid value for param "table-title-placement"&#xA;</xsl:text>
                <xsl:text>Valid values are 'top', and 'bottom'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test= "$footnote-style != 'list'  and $footnote-style != 'traditional'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$footnote-style"/>
                <xsl:text>" not a valid value for param "footnote-style"&#xA;</xsl:text>
                <xsl:text>Valid values are 'list', and 'traditional'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test= "$footnote-placement != 'footnote'  and $footnote-placement != 'endnote'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$footnote-placement"/>
                <xsl:text>" not a valid value for param "footnote-placement"&#xA;</xsl:text>
                <xsl:text>Valid values are 'footnote', and 'endnote'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test= "$internal-link-type != 'link'  and $internal-link-type != 'page'
                and $internal-link-type != 'page-link'">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$internal-link-type"/>
                <xsl:text>" not a valid value for param "internal-link-type"&#xA;</xsl:text>
                <xsl:text>Valid values are 'link', and 'page', and 'page-link'&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test= "$bibliographic-format != 'list'  and $bibliographic-format != 'normal'
            and $bibliographic-format != ''">
            <xsl:variable name="msg">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$bibliographic-format"/>
                <xsl:text>" not a valid value for param "bibliographic-format"&#xA;</xsl:text>
                <xsl:text>Valid values are 'list', 'normal', or ''.&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>

        <!--test $font-order parameter for valid values-->
        <xsl:call-template name="test-order">
            <xsl:with-param name="order" select="$front-order"/>
        </xsl:call-template>

        <!--test $front order to make sure appropriate values included-->
        <xsl:if test = "$title-exists and not(contains($front-order, 'title'))">
            <xsl:variable name="msg">
                <xsl:text>'title' has not been declared in parameter 'front-order', </xsl:text>
                <xsl:text>yet this element exists.&#xA;</xsl:text>
                <xsl:text>Please fix. (For example, 'title, bibliographic, dedication, abstract, toc')&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test = "$bibliographic-exists and not(contains($front-order, 'bibliographic'))">
            <xsl:variable name="msg">
                <xsl:text>'bibliographic' has not been declared in parameter 'front-order', </xsl:text>
                <xsl:text>yet this element exists.&#xA;</xsl:text>
                <xsl:text>Please fix. (For example, 'title, bibliographic, dedication, abstract, toc')&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test = "$dedication-exists and not(contains($front-order, 'dedication'))">
            <xsl:variable name="msg">
                <xsl:text>'dedication' has not been declared in parameter 'front-order', </xsl:text>
                <xsl:text>yet this element exists.&#xA;</xsl:text>
                <xsl:text>Please fix. (For example, 'title, bibliographic, dedication, abstract, toc')&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test = "$abstract-exists and not(contains($front-order, 'abstract'))">
            <xsl:variable name="msg">
                <xsl:text>'abstract' has not been declared in parameter 'front-order', </xsl:text>
                <xsl:text>yet this element exists.&#xA;</xsl:text>
                <xsl:text>Please fix. (For example, 'title, bibliographic, dedication, abstract, toc')&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
        <xsl:if test = "$toc-exists and not(contains($front-order, 'toc'))">
            <xsl:variable name="msg">
                <xsl:text>'toc' has not been declared in parameter 'front-order', </xsl:text>
                <xsl:text>yet this element exists.&#xA;</xsl:text>
                <xsl:text>Please fix. (For example, 'title, bibliographic, dedication, abstract, toc')&#xA;</xsl:text>
            </xsl:variable>
            <xsl:call-template name="quit-message">
                <xsl:with-param name="msg" select="$msg"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="test-order">
        <xsl:param name="order"/>
        <xsl:param name="first-run">True</xsl:param>
        <xsl:variable name="order-string">
            <xsl:choose>
                <xsl:when test="$first-run='True'">
                    <xsl:value-of select="concat($order, ',')"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$order"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="matter" select="normalize-space(substring-before($order-string, ','))"/>
        <xsl:if test="$matter != ''">
            <xsl:if test = "$matter != 'title' and $matter != 'bibliographic' and 
                $matter != 'dedication' and $matter != 'abstract' and $matter != 'toc'">
                <xsl:variable name="msg">
                    <xsl:text>"</xsl:text>
                    <xsl:value-of select="$matter"/>
                    <xsl:text>" not a valid area for parameter 'front-order'&#xA;</xsl:text>
                    <xsl:text>Valid values are 'title', 'bibliographic', 'dedication',</xsl:text>
                    <xsl:text> 'abstract', and 'toc'&#xA;</xsl:text>
                </xsl:variable>
                <xsl:call-template name="quit-message">
                    <xsl:with-param name="msg" select="$msg"/>
                </xsl:call-template>
            </xsl:if>
            <xsl:call-template name="test-order">
                <xsl:with-param name="order" select="substring-after($order-string, ',')"/>
                <xsl:with-param name="first-run" select="'False'"/>
            </xsl:call-template>
        </xsl:if>
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
        <xsl:text>[</xsl:text>
        <xsl:for-each select="@*">
            <xsl:value-of select="name(.)"/>
            <xsl:text>="</xsl:text>
            <xsl:value-of select="."/>
            <xsl:text>" </xsl:text>
        </xsl:for-each>
        <xsl:text>]</xsl:text>
        <xsl:text>&#xA;</xsl:text>
        <xsl:value-of select="$siblings"/>
    </xsl:template>


    <xsl:template match="*">
        <xsl:variable name="trace">
            <xsl:call-template name="trace"/>
        </xsl:variable>
        <xsl:message>
            <xsl:text>no match for </xsl:text>
            <xsl:value-of select="$trace"/>
        </xsl:message>
        <xsl:choose>
            <xsl:when test="$strict='True'">
                <xsl:message terminate="yes">
                    <xsl:text>Processing XSLT Stylesheets now quiting</xsl:text>
                </xsl:message>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Not processing text in this element.</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
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

    <xsl:template name="quit-message">
        <xsl:param name="msg"/>
        <xsl:message terminate="yes">
            <xsl:value-of select="$msg"/>
            <xsl:text>Processing stylesheets now quitting.</xsl:text>
        </xsl:message>
    </xsl:template>

    <xsl:template name="error-message">
        <xsl:param name="text"/>
        <xsl:message>
            <xsl:value-of select="$text"/>
        </xsl:message>
        <xsl:choose>
            <xsl:when test="$strict='True'">
                <xsl:call-template name="quit-message"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Not processing text for this element.</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    
</xsl:stylesheet>
