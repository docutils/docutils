<!--WARNING TO PAUL: DON'T EDIT THIS FILE use update.py instead.
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                             version="1.0">

<!--

change the file name string into individual fiels for easier transformation later


-->

 <xsl:template match="field_name">
  <field_name>
   <xsl:call-template name="tokens"/>
  </field_name> 
 </xsl:template>

<xsl:template name="tokens">
  <xsl:param name="str" select="normalize-space(.)"/>
  <xsl:param name="sep" select="' '"/>
  <xsl:choose>
   <xsl:when test="contains($str,' ')">
   <xsl:call-template name="arguments">
    <xsl:with-param name="str" select="substring-before($str,' ')"/>
   </xsl:call-template>
   <xsl:value-of select="$sep"/>
   <xsl:call-template name="tokens">
    <xsl:with-param name="str" select="substring-after($str,' ')"/>
   </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
       <xsl:call-template name="arguments">
    <xsl:with-param name="str" select="$str"/>
   </xsl:call-template>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

 <xsl:template name= "arguments">
  <xsl:param name="str" select="string(.)"/>
     <arguments>
         <xsl:value-of select = '$str'/>
     </arguments>
 </xsl:template>



                             
    <xsl:template match="@*|node()">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
