
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
    <!-- $Date: 2011-01-09 02:51:33 -0500 (Sun, 09 Jan 2011) $ -->
<!--create new page sequences for sections one level deep, including page breaks-->

<!--if set to True, stylesheets cause quit when error found-->
<xsl:param name="strict"/>

<!--wheter to start new page when coming across sections one level deep
Not implemented yet-->
<xsl:param name="create-chapters"/>

<!--layout is either 
simple (all pages the same)

first (different first page from rest)

odd-even (different for odd and even; headers and footers will be the same because of the limitations of rst)

first-odd-even (different first, odd, and even; headers and footers will be the same, with the option of suppressing the first header and footer)

-->
<xsl:param name="page-layout"/>


<!--supress the header on the first page--> 
<xsl:param name="suppress-first-page-header"/>

<!--supress the footer on the first page--> 
<xsl:param name="suppress-first-page-footer"/>

<!--these are unfortunate parameters; they should be set to exactly the value of extent in 
the page-header or page-footer attribute sets-->
<xsl:param name="spacing-header"/>
<xsl:param name="spacing-footer"/>

<!--text to use for transition elements-->
<xsl:param name="transition-text">***</xsl:param>
    
</xsl:stylesheet>
