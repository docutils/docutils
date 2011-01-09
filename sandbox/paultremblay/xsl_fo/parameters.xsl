
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.1"
    >
<!--create new page sequences for sections one level deep, including page breaks-->
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
    
</xsl:stylesheet>
