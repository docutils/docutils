<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:g = "http://relaxng.org/ns/structure/1.0"
    version="1.1"
    >
    
    <xsl:template match="/g:grammar">
        <root>
          <xsl:apply-templates/>
        </root>
    </xsl:template>

<!--
/g:grammar/g:define[@name = 'root.attlist']|
/g:grammar/g:define[@name = 'meta-info.attlist']|
/g:grammar/g:define[@name = 'meta-field.attlist']|
/g:grammar/g:define[@name = 'declarations.attlist']|
/g:grammar/g:define[@name = 'color-profile.attlist']|
/g:grammar/g:define[@name = 'outline.attlist']|
/g:grammar/g:define[@name = 'bookmark.attlist']|
/g:grammar/g:define[@name = 'bookmark-label.attlist']|
/g:grammar/g:define[@name = 'layout-master-set.attlist']|
/g:grammar/g:define[@name = 'page-sequence-master.attlist']|
/g:grammar/g:define[@name = 'single-page-master-reference.attlist']|
/g:grammar/g:define[@name = 'repeatable-page-master-reference.attlist']|
/g:grammar/g:define[@name = 'repeatable-page-master-alternatives.attlist']|
/g:grammar/g:define[@name = 'conditional-page-master-reference.attlist']|
/g:grammar/g:define[@name = 'simple-page-master.attlist']|
/g:grammar/g:define[@name = 'region-body.attlist']|
/g:grammar/g:define[@name = 'region-before.attlist']|
/g:grammar/g:define[@name = 'region-after.attlist']|
/g:grammar/g:define[@name = 'region-start.attlist']|
/g:grammar/g:define[@name = 'region-end.attlist']|
/g:grammar/g:define[@name = 'page-sequence.attlist']|
/g:grammar/g:define[@name = 'title.attlist']|
/g:grammar/g:define[@name = 'flow.attlist']|
/g:grammar/g:define[@name = 'static-content.attlist']|
/g:grammar/g:define[@name = 'flow-section.attlist']|
/g:grammar/g:define[@name = 'block.attlist']|
/g:grammar/g:define[@name = 'absolute-container.attlist']|
/g:grammar/g:define[@name = 'bidi-override.attlist']|
/g:grammar/g:define[@name = 'character.attlist']|
/g:grammar/g:define[@name = 'initial-property-set.attlist']|
/g:grammar/g:define[@name = 'external-graphic.attlist']|
/g:grammar/g:define[@name = 'instream-foreign-object.attlist']|
/g:grammar/g:define[@name = 'inline.attlist']|
/g:grammar/g:define[@name = 'inline-container.attlist']|
/g:grammar/g:define[@name = 'leader.attlist']|
/g:grammar/g:define[@name = 'page-number.attlist']|
/g:grammar/g:define[@name = 'page-number-citation.attlist']|
/g:grammar/g:define[@name = 'begin-index-range.attlist']|
/g:grammar/g:define[@name = 'end-index-range.attlist']|
/g:grammar/g:define[@name = 'page-index.attlist']|
/g:grammar/g:define[@name = 'index-item.attlist']|
/g:grammar/g:define[@name = 'table-and-caption.attlist']|
/g:grammar/g:define[@name = 'table-caption.attlist']|
/g:grammar/g:define[@name = 'table.attlist']|
/g:grammar/g:define[@name = 'table-column.attlist']|
/g:grammar/g:define[@name = 'row-group.attlist']|
/g:grammar/g:define[@name = 'table-header.attlist']|
/g:grammar/g:define[@name = 'table-footer.attlist']|
/g:grammar/g:define[@name = 'table-body.attlist']|
/g:grammar/g:define[@name = 'table-row.attlist']|
/g:grammar/g:define[@name = 'table-cell.attlist']|
/g:grammar/g:define[@name = 'list-block.attlist']|
/g:grammar/g:define[@name = 'list-item.attlist']|
/g:grammar/g:define[@name = 'list-item-label.attlist']|
/g:grammar/g:define[@name = 'list-item-body.attlist']|
/g:grammar/g:define[@name = 'side-float.attlist']|
/g:grammar/g:define[@name = 'before-float.attlist']|
/g:grammar/g:define[@name = 'footnote.attlist']|
/g:grammar/g:define[@name = 'footnote-body.attlist']|
/g:grammar/g:define[@name = 'basic-link.attlist']|
/g:grammar/g:define[@name = 'wrapper.attlist']|
/g:grammar/g:define[@name = 'marker.attlist']|
/g:grammar/g:define[@name = 'retrieve-marker.attlist']|
/g:grammar/g:define[@name = 'multi-switch.attlist']|
/g:grammar/g:define[@name = 'multi-case.attlist']|
/g:grammar/g:define[@name = 'multi-toggle.attlist']|
/g:grammar/g:define[@name = 'multi-properties.attlist']|
/g:grammar/g:define[@name = 'multi-property-set.attlist']|
-->


    <xsl:template match="/g:grammar/g:define[@name = 'block.attlist']|
        /g:grammar/g:define[@name = 'block-container.attlist']|
        /g:grammar/g:define[@name = 'table-cell.attlist']|
        /g:grammar/g:define[@name = 'flow.attlist']|
        /g:grammar/g:define[@name = 'footnote-body.attlist']|
        /g:grammar/g:define[@name = 'table-header.attlist']|
        /g:grammar/g:define[@name = 'inline.attlist']|
        /g:grammar/g:define[@name = 'list-item-body.attlist']|
        /g:grammar/g:define[@name = 'list-item-label.attlist']|
        /g:grammar/g:define[@name = 'list-block.attlist']|
        /g:grammar/g:define[@name = 'list-item.attlist']|
        /g:grammar/g:define[@name = 'page-sequence.attlist']|
        /g:grammar/g:define[@name = 'region-after.attlist']|
        /g:grammar/g:define[@name = 'region-before.attlist']|
        /g:grammar/g:define[@name = 'region-body.attlist']|
        /g:grammar/g:define[@name = 'simple-page-master.attlist']|
        /g:grammar/g:define[@name = 'table.attlist']|
        /g:grammar/g:define[@name = 'table-body.attlist']|
        /g:grammar/g:define[@name = 'table-row.attlist'] " priority = "2">

        
      <define name="{@name}">
            <xsl:apply-templates  mode = "use"/>
         </define>
    </xsl:template>

     <xsl:template match="g:ref" mode="use">
         <xsl:variable name="name" select = "@name"/>
        <xsl:apply-templates select="/g:grammar/g:define[@name = $name]" mode="use"/>
    </xsl:template>

    <xsl:template match = "g:define" mode="use">
        <xsl:variable name="name" select="@name"/>
        <xsl:choose>
            <xsl:when test="contains($name, '.attr')">
                <xsl:apply-templates  mode = "use"/>
            </xsl:when>
            <xsl:otherwise>
                <define name="{@name}">
                    <xsl:apply-templates  mode = "use"/>
                </define>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="g:optional" priority="2" mode="use">
        <xsl:apply-templates mode="use"/>
    </xsl:template>

    <xsl:template match="g:attribute" priority="2" mode="use">
        <attribute name="{@name}"/>
    </xsl:template>

    <xsl:template match="@*|node()"/>


</xsl:stylesheet>
