import os, sys
TEST = False
num_styles = 30
if TEST:
    en = 3
else:
    en =  num_styles + 1

ss = '/Users/cynthia/tmp/paultremblay/docutilsToFo/xsl_fo/table_extended.xsl'
write_obj = file(ss, 'w')
start= """<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >

    <!-- $Id$ -->\n\n"""

write_obj.write(start)
# write attribute sets

for n in range(1,en):
    s = """\n     <xsl:attribute-set name="table%s-block-container">
        <xsl:attribute name="space-before">12pt</xsl:attribute>
        <xsl:attribute name="space-after">12pt</xsl:attribute>
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s">
        <xsl:attribute name="table-layout">fixed</xsl:attribute>
        <xsl:attribute name="inline-progression-dimension">100%%</xsl:attribute>
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s-header">
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)
    
    s = """\n     <xsl:attribute-set name="table%s-header-cell">
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s-header-block">
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s-body">
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s-header-row">
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s-row">
        <xsl:attribute name="keep-together.within-page">always</xsl:attribute>
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="table%s-cell" >
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

    s = """\n     <xsl:attribute-set name="cell%s-block">
    </xsl:attribute-set>\n""" % (n)
    write_obj.write(s)

s = """\n     <!--END OF ATTRIBUTE SETS-->\n""" 
write_obj.write(s)

s = """\n     <xsl:template name="make-fo-table-cols">
         <xsl:param name="cols-string"/>
         <xsl:param name="col-num">1</xsl:param>
         <xsl:variable name="before-comma" 
            select="normalize-space(substring-before($cols-string, ','))" />
         <xsl:variable name="col-width">
             <xsl:choose>
                 <xsl:when test="$before-comma">
                     <xsl:value-of select="$before-comma"/>
                 </xsl:when>
                 <xsl:otherwise>
                     <xsl:value-of select="$cols-string"/>
                 </xsl:otherwise>
             </xsl:choose>
         </xsl:variable>
         <xsl:choose>
             <xsl:when test="$col-width != ''">
                <fo:table-column column-number="{$col-num}" 
                    column-width="proportional-column-width({$col-width})"/>
                <xsl:call-template name="make-fo-table-cols">
                    <xsl:with-param name="cols-string" 
                        select= "normalize-space(substring-after($cols-string, ','))"/>
                    <xsl:with-param name="col-num" select="$col-num + 1"/>
                </xsl:call-template>
             </xsl:when>
         </xsl:choose>
    </xsl:template>\n"""
write_obj.write(s)

s= """\n     <xsl:template name="get-column-widths">
        <xsl:param name="classes"/>
        <xsl:variable name="columns">
            <xsl:choose>"""

write_obj.write(s)

for n in range(1,en):
    s= """\n                 <xsl:when test="$classes = 'table%s'">
                    <xsl:value-of select="$table%s-cols"/>
                </xsl:when>\n""" % (n, n)
    write_obj.write(s)

s= """\n                 <xsl:when test="$classes = 'borderless'">
                       <xsl:value-of select="$table-borderless-cols"/>
                  </xsl:when> 

                  <xsl:when test="$classes = 'long'">
                       <xsl:value-of select="$table-long-cols"/>
                  </xsl:when> 

                  <xsl:when test="$classes = ''">
                       <xsl:value-of select="$table-cols"/>
                  </xsl:when> 

             </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$columns"/>
    </xsl:template>\n"""

write_obj.write(s)

s = """\n     
    <xsl:template name="make-col-specs">
        <xsl:param name="classes"/>
        <xsl:variable name="columns">
            <xsl:call-template name = "get-column-widths">
                <xsl:with-param name="classes" select = "$classes"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$columns != ''">
                <xsl:call-template name="make-fo-table-cols">
                    <xsl:with-param name="cols-string" select="$columns"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates select="tgroup/colspec" mode="use">
                    <xsl:with-param name="classe" select="$classes"/>
                </xsl:apply-templates>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="cols-spanned">
        <xsl:choose>
            <xsl:when test="@morecols">
                <xsl:value-of select="@morecols + 1"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="rows-spanned">
        <xsl:choose>
            <xsl:when test="@morerows">
                <xsl:value-of select="@morerows + 1"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>\n"""

write_obj.write(s)

s= """\n     <xsl:template match="tgroup/colspec" mode="use">
        <xsl:variable name="col-num">
            <xsl:number/>
        </xsl:variable>
        <fo:table-column column-number="{$col-num}" 
            column-width="proportional-column-width({@colwidth})"/>
     </xsl:template>

     <xsl:template match="tgroup" >
         <xsl:param name="classes"/>
         <xsl:apply-templates >
             <xsl:with-param name="classes" select="$classes"/>
         </xsl:apply-templates>
    </xsl:template>\n"""

write_obj.write(s)

for n in range(1,en):
    s = """\n   
     <xsl:template match="table[@classes='table%s']">
        <fo:block-container xsl:use-attribute-sets = "table%s-block-container">
            <xsl:if test="title and $table-title-placement = 'top'">
                <xsl:apply-templates select="title" />
            </xsl:if>
            <fo:table role="table%s" xsl:use-attribute-sets="table%s">
                <xsl:call-template name="make-col-specs">
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:call-template>
                <xsl:apply-templates >
                    <xsl:with-param name="classes" select="@classes"/>
                </xsl:apply-templates>
            </fo:table>
            <xsl:if test="title and $table-title-placement = 'bottom'">
                <xsl:apply-templates select="title" />
            </xsl:if>
        </fo:block-container>
    </xsl:template>


    <xsl:template match="table[@classes = 'table%s']/tgroup/thead" >
        <fo:table-header xsl:use-attribute-sets = "table%s-header">
            <xsl:apply-templates />
        </fo:table-header>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup//thead/row" >
        <fo:table-row xsl:use-attribute-sets="table%s-header-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup/thead/row/entry" >
        <fo:table-cell xsl:use-attribute-sets="table%s-header-cell">
            <xsl:apply-templates />
            <xsl:if test="not(paragraph)">
                <fo:block/>
            </xsl:if>
        </fo:table-cell>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup/thead/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="table%s-header-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup/tbody" >
        <fo:table-body xsl:use-attribute-sets="table%s-body">
            <xsl:apply-templates />
        </fo:table-body>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup/tbody/row" >
        <fo:table-row xsl:use-attribute-sets="table%s-row">
            <xsl:apply-templates />
        </fo:table-row>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup/tbody/row/entry" >
        <xsl:variable name="cols-spanned">
            <xsl:call-template name="cols-spanned"/>
        </xsl:variable>
        <xsl:variable name="rows-spanned">
            <xsl:call-template name="rows-spanned"/>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$cols-spanned= 1 and $rows-spanned = 1">
                <fo:table-cell xsl:use-attribute-sets="table%s-cell">
                    <xsl:apply-templates/>
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$cols-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table%s-cell"
                    number-rows-spanned="{$rows-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:when test="$rows-spanned= 1">
                <fo:table-cell xsl:use-attribute-sets="table%s-cell"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:when>
            <xsl:otherwise>
                <fo:table-cell xsl:use-attribute-sets="table%s-cell"
                    number-rows-spanned="{$rows-spanned}"
                    number-columns-spanned="{$cols-spanned}">
                    <xsl:apply-templates />
                    <xsl:if test="not(paragraph)">
                        <fo:block/>
                    </xsl:if>
                </fo:table-cell>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="table[@classes='table%s']/tgroup/tbody/row/entry/paragraph" >
        <fo:block xsl:use-attribute-sets="cell%s-block">
            <xsl:apply-templates/>
        </fo:block>
    </xsl:template>\n""" % (n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n)

    write_obj.write(s)

s ="""\n     <xsl:template match="title" mode="classes"/>\n\n"""
write_obj.write(s)

end = '</xsl:stylesheet>'
write_obj.write(end)

write_obj.close()
