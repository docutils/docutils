<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1">

    <!-- $Id$ -->

    <xsl:output method="text"/>

    <xsl:template match="xsl:stylesheet">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="/">
        <xsl:text>att_set_dict = {&#xA;</xsl:text>

            <xsl:apply-templates select="document('../xsl_fo/root.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/page.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/bibliographic_fields.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/front_matter.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/header_footer.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/toc.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/section.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/body_elements.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/bullet_list.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/enumerated_list.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/definition_list.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/field_list.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/option_list.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/line_block.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/table.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/footnote.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/admonitions.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/image_figure.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/body_directives.xsl')/xsl:stylesheet"/>
            <xsl:apply-templates select="document('../xsl_fo/inline.xsl')/xsl:stylesheet"/>
            <xsl:text>}</xsl:text>
    </xsl:template>

    <xsl:template match="xsl:attribute-set" priority="2">
        <xsl:variable name="name" select="@name"/>
        <xsl:choose>
            <xsl:when test="substring($name, string-length($name) -5) = '-block'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'block'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -13) = '-page-sequence'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'page-sequence'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -4) = '-flow'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'flow'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -6) = '-inline'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'inline'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -9) = '-list-item'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'list-item'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -10) = '-item-label'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'item-label'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -9) = '-item-body'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'item-body'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -15) = '-block-container'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'block-container'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -4) = 'table'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'table'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -11) = 'table-header'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'table-header'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -3) = 'cell'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'cell'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -9) = 'table-body'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'table-body'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -8) = 'table-row'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'table-row'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -10) = 'region-body'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'region-body'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -5) = 'header'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'header'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -12) = 'footnote-body'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'footnote-body'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -17) = 'simple-page-master'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'simple-page-master'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -12) = 'region-before'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'region-before'],&#xA;</xsl:text>
            </xsl:when>
            <xsl:when test="substring($name, string-length($name) -11) = 'region-after'">
                <xsl:text>'</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>':['</xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>', 'region-after'],&#xA;</xsl:text>
            </xsl:when>

            <xsl:otherwise>
                <xsl:text>??? </xsl:text>
                <xsl:value-of select="$name"/>
                <xsl:text>&#xA;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="@*|node()"/>

    
</xsl:stylesheet>
