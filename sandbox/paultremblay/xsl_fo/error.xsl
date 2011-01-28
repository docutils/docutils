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
                <xsl:text>" not a valid value for param "front-matter-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'own-section', 'with-toc' or 'with-body'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>

        <xsl:if test= "$toc-pagination != 'own-section'  and $toc-pagination != 'with-body'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$toc-pagination"/>
                <xsl:text>" not a valid value for param "toc-pagination"&#xA;</xsl:text>
                <xsl:text>Valid values are 'own-section', or 'with-body'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>

        <xsl:if test= "$page-layout != 'simple'  and $page-layout != 'odd-even' 
            and $page-layout != 'first-odd-even' and $page-layout != 'first'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for param "page-layout"&#xA;</xsl:text>
                <xsl:text>Valid values are 'simple', 'odd-even', 'first', or 'first-odd-even'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test= "$option-list-format != 'list'  and $option-list-format != 'definition'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$option-list-format"/>
                <xsl:text>" not a valid value for param "option-list-format"&#xA;</xsl:text>
                <xsl:text>Valid values are 'list', and 'definition'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test ="$number-verse != '' and string($number-verse + 1 ) = 'NaN'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$number-verse"/>
                <xsl:text>" not a valid value for param "number-verse"&#xA;</xsl:text>
                <xsl:text>Please use a number&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test= "$table-title-placement != 'bottom'  and $table-title-placement != 'top'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$table-title-placement"/>
                <xsl:text>" not a valid value for param "table-title-placement"&#xA;</xsl:text>
                <xsl:text>Valid values are 'top', and 'bottom'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test= "$footnote-style != 'list'  and $footnote-style != 'traditional'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$footnote-style"/>
                <xsl:text>" not a valid value for param "footnote-style"&#xA;</xsl:text>
                <xsl:text>Valid values are 'list', and 'traditional'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <xsl:if test= "$footnote-placement != 'footnote'  and $footnote-placement != 'endnote'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$footnote-placement"/>
                <xsl:text>" not a valid value for param "footnote-placement"&#xA;</xsl:text>
                <xsl:text>Valid values are 'footnote', and 'endnote'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        <!--
        <xsl:if test= "$document-title != 'own-page'  and $document-title != 'not-own-page'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$option-list-format"/>
                <xsl:text>" not a valid value for param "document-title"&#xA;</xsl:text>
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

    <xsl:template name="error-message">
        <xsl:param name="text"/>
        <xsl:message>
            <xsl:value-of select="$text"/>
        </xsl:message>
        <xsl:choose>
            <xsl:when test="$strict='True'">
                <xsl:message terminate="yes">
                    <xsl:text>Processing stylesheets now quitting.</xsl:text>
                </xsl:message>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message>
                    <xsl:text>Not processing text for this element.</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    
</xsl:stylesheet>
