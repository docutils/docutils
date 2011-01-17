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
            and $page-layout != 'first-odd-even'">
            <xsl:message terminate = "yes">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="$page-layout"/>
                <xsl:text>" not a valid value for parm "page-layout"&#xA;</xsl:text>
                <xsl:text>Valid values are 'simple', 'odd-even', 'first', or 'first-odd-even'&#xA;</xsl:text>
                <xsl:text>Processing XSLT now quiting.</xsl:text>
            </xsl:message>
        </xsl:if>
        
    </xsl:template>
    
</xsl:stylesheet>
