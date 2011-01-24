<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Id$ -->

    <xsl:template name="test-params">
        <xsl:if test= "$front-matter-pagination != 'own-section' and $front-matter-pagination != 'with-toc' 
            and $front-matter-pagination != 'with-body'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$front-matter-pagination"/>
                <xsl:text>" not a valid value for parm "front-matter-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'own-section', 'with-toc' or 'with-body'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>

        <xsl:if test= "$toc-pagination != 'own-section'  and $toc-pagination != 'with-body'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$toc-pagination"/>
                <xsl:text>" not a valid value for parm "toc-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'own-section', or 'with-body'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>

        <xsl:if test= "$page-layout != 'simple'  and $page-layout != 'odd-even' 
            and $page-layout != 'first-odd-even' and $page-layout != 'first'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for parm "page-layout"&#xA;</xsl:text>
                <xsl:text>Valid values are 'simple', 'odd-even', 'first', or 'first-odd-even'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test= "$option-list-format != 'list'  and $option-list-format != 'definition'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$option-list-format"/>
                <xsl:text>" not a valid value for parm "option-list-format"&#xA;</xsl:text>
                <xsl:text>Valid values are 'list', and 'definition'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <!--
        <xsl:if test= "$document-title != 'own-page'  and $document-title != 'not-own-page'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$option-list-format"/>
                <xsl:text>" not a valid value for parm "document-title"&#xA;</xsl:text>
                <xsl:text>Valid values are 'own-page', and 'not-own-page'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        -->
    </xsl:template>

    <xsl:template match="*">
        <xsl:message>
            <xsl:text>no match for </xsl:text>
            <xsl:value-of select="name(.)"/>
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

    
</xsl:stylesheet>
