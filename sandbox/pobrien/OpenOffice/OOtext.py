"""Text used in the creation of the XML files."""

__author__ = "Patrick K. O'Brien <pobrien@orbtech.com>"
__cvsid__ = "$Id$"
__revision__ = "$Revision$"[11:-2]


# Can't have blank line at beginning of XML

manifest = '''<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE manifest:manifest PUBLIC "-//OpenOffice.org//DTD Manifest 1.0//EN" "Manifest.dtd">
<manifest:manifest xmlns:manifest="http://openoffice.org/2001/manifest">
<manifest:file-entry manifest:media-type="application/vnd.sun.xml.writer" manifest:full-path="/"/>
<manifest:file-entry manifest:media-type="" manifest:full-path="Pictures/"/>
%s
</manifest:manifest>
'''

manifest_format = '<manifest:file-entry manifest:media-type="text/xml" manifest:full-path="%s"/>'

content_header = '''<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE office:document-content PUBLIC "-//OpenOffice.org//DTD OfficeDocument 1.0//EN" "office.dtd">
<office:document-content
xmlns:office="http://openoffice.org/2000/office"
xmlns:style="http://openoffice.org/2000/style"
xmlns:text="http://openoffice.org/2000/text"
xmlns:table="http://openoffice.org/2000/table"
xmlns:draw="http://openoffice.org/2000/drawing"
xmlns:fo="http://www.w3.org/1999/XSL/Format"
xmlns:xlink="http://www.w3.org/1999/xlink"
xmlns:number="http://openoffice.org/2000/datastyle"
xmlns:svg="http://www.w3.org/2000/svg"
xmlns:chart="http://openoffice.org/2000/chart"
xmlns:dr3d="http://openoffice.org/2000/dr3d"
xmlns:math="http://www.w3.org/1998/Math/MathML"
xmlns:form="http://openoffice.org/2000/form"
xmlns:script="http://openoffice.org/2000/script" office:class="text"
office:version="1.0">
<office:body>
<text:sequence-decls>
<text:sequence-decl text:display-outline-level="0" text:name="Illustration"/>
<text:sequence-decl text:display-outline-level="0" text:name="Table"/>
<text:sequence-decl text:display-outline-level="0" text:name="Text"/>
<text:sequence-decl text:display-outline-level="0" text:name="Drawing"/>
</text:sequence-decls>
'''

content_footer = '''
</office:body>
</office:document-content>
'''

styles = '''<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE office:document-styles PUBLIC "-//OpenOffice.org//DTD OfficeDocument 1.0//EN" "office.dtd">

<office:document-styles
xmlns:office="http://openoffice.org/2000/office"
xmlns:style="http://openoffice.org/2000/style"
xmlns:text="http://openoffice.org/2000/text"
xmlns:table="http://openoffice.org/2000/table"
xmlns:draw="http://openoffice.org/2000/drawing"
xmlns:fo="http://www.w3.org/1999/XSL/Format"
xmlns:xlink="http://www.w3.org/1999/xlink"
xmlns:number="http://openoffice.org/2000/datastyle"
xmlns:svg="http://www.w3.org/2000/svg"
xmlns:chart="http://openoffice.org/2000/chart"
xmlns:dr3d="http://openoffice.org/2000/dr3d"
xmlns:math="http://www.w3.org/1998/Math/MathML"
xmlns:form="http://openoffice.org/2000/form"
xmlns:script="http://openoffice.org/2000/script"
office:version="1.0">

<office:font-decls>

<style:font-decl style:name="Wingdings"
fo:font-family="Wingdings"
style:font-pitch="variable"
style:font-charset="x-symbol"/>
<style:font-decl style:name="Palatino"
fo:font-family="Palatino"/>
<style:font-decl style:name="Courier"
fo:font-family="Courier"
style:font-family-generic="modern"/>
<style:font-decl style:name="Arial Unicode MS"
fo:font-family="&apos;Arial Unicode MS&apos;"
style:font-pitch="variable"/>
<style:font-decl style:name="HG Mincho Light J"
fo:font-family="&apos;HG Mincho Light J&apos;"
style:font-pitch="variable"/>
<style:font-decl style:name="AGaramond"
fo:font-family="AGaramond"
style:font-family-generic="roman"
style:font-pitch="variable"/>
<style:font-decl style:name="AGaramond Bold"
fo:font-family="&apos;AGaramond Bold&apos;"
style:font-family-generic="roman"
style:font-pitch="variable"/>
<style:font-decl style:name="Times"
fo:font-family="Times"
style:font-family-generic="roman"
style:font-pitch="variable"/>
<style:font-decl style:name="Times New Roman"
fo:font-family="&apos;Times New Roman&apos;"
style:font-family-generic="roman"
style:font-pitch="variable"/>
<style:font-decl style:name="Arial"
fo:font-family="Arial"
style:font-family-generic="swiss"
style:font-pitch="variable"/>
<style:font-decl style:name="Helvetica"
fo:font-family="Helvetica"
style:font-family-generic="swiss"
style:font-pitch="variable"/>
<style:font-decl style:name="Helvetica-Narrow"
fo:font-family="Helvetica-Narrow"
style:font-family-generic="swiss"
style:font-pitch="variable"/>
<style:font-decl style:name="Humanst521 Cn BT"
fo:font-family="&apos;Humanst521 Cn BT&apos;"
style:font-family-generic="swiss"
style:font-pitch="variable"/>
<style:font-decl style:name="Luxi Sans"
fo:font-family="&apos;Luxi Sans&apos;"
style:font-family-generic="swiss"
style:font-pitch="variable"/>
<style:font-decl style:name="Tahoma"
fo:font-family="Tahoma"
style:font-family-generic="swiss"
style:font-pitch="variable"/>
<style:font-decl style:name="Univers 45 Light"
fo:font-family="&apos;Univers 45 Light&apos;"
style:font-family-generic="swiss"
style:font-pitch="variable"/>

</office:font-decls>
<office:styles>


<style:default-style style:family="graphics">
<style:properties draw:start-line-spacing-horizontal="0.1114inch"
draw:start-line-spacing-vertical="0.1114inch"
draw:end-line-spacing-horizontal="0.1114inch"
draw:end-line-spacing-vertical="0.1114inch" fo:color="#000000"
style:font-name="Palatino" fo:font-size="12pt" fo:language="en"
fo:country="US" style:font-name-asian="HG Mincho Light J"
style:font-size-asian="12pt" style:language-asian="none"
style:country-asian="none" style:font-name-complex="Arial Unicode MS"
style:font-size-complex="12pt" style:language-complex="none"
style:country-complex="none" style:text-autospace="ideograph-alpha"
style:punctuation-wrap="simple" style:line-break="strict">
<style:tab-stops/></style:properties></style:default-style>

<style:default-style style:family="paragraph">
<style:properties fo:color="#000000" style:font-name="Palatino"
fo:font-size="12pt" fo:language="en" fo:country="US"
style:font-name-asian="HG Mincho Light J" style:font-size-asian="12pt"
style:language-asian="none" style:country-asian="none"
style:font-name-complex="Arial Unicode MS"
style:font-size-complex="12pt" style:language-complex="none"
style:country-complex="none" fo:hyphenate="false"
fo:hyphenation-remain-char-count="2"
fo:hyphenation-push-char-count="2"
fo:hyphenation-ladder-count="no-limit"
style:text-autospace="ideograph-alpha"
style:punctuation-wrap="hanging" style:line-break="strict"
style:tab-stop-distance="0.5inch"/></style:default-style>

<style:style style:name="code" 
style:family="text">
<style:properties style:font-name="Courier"/></style:style>

<style:style style:name="Standard" 
style:family="paragraph"
style:class="text">
<style:properties fo:color="#000000"
style:font-name="Times New Roman" fo:font-size="10pt" fo:language="en"
fo:country="US" style:font-name-asian="Times New Roman"
style:font-size-asian="10pt" fo:orphans="2"
fo:widows="2"/></style:style>

<style:style style:name="Text body" 
style:family="paragraph" 
style:parent-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0inch"
fo:margin-bottom="0.0835inch"/></style:style>

<style:style style:name="Heading" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Text body"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0835inch" style:font-name="Luxi Sans"
fo:font-size="14pt" style:font-name-asian="HG Mincho Light J"
style:font-size-asian="14pt" style:font-name-complex="Arial Unicode
MS" style:font-size-complex="14pt"
fo:keep-with-next="true"/></style:style>

<style:style style:name="Heading 1" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" style:font-name="Arial"
fo:font-size="14pt" fo:font-weight="bold" style:letter-kerning="true"
style:font-size-asian="14pt" style:font-weight-asian="bold"
fo:keep-with-next="true"/></style:style>

<style:style style:name="Heading 3" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" style:font-name="Arial"
fo:font-size="12pt" style:font-size-asian="12pt"
fo:keep-with-next="true"/></style:style>

<style:style style:name="Heading 4" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" style:font-name="Arial"
fo:font-size="12pt" fo:font-weight="bold" style:font-size-asian="12pt"
style:font-weight-asian="bold"
fo:keep-with-next="true"/></style:style>

<style:style style:name="Heading 5" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" fo:font-size="11pt"
style:font-size-asian="11pt"/></style:style>

<style:style style:name="Heading 6" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" fo:font-size="11pt"
fo:font-style="italic" style:font-size-asian="11pt"
style:font-style-asian="italic"/></style:style>

<style:style style:name="Heading 7" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch"
style:font-name="Arial"/></style:style>

<style:style style:name="Heading 8" 
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" style:font-name="Arial"
fo:font-style="italic"
style:font-style-asian="italic"/></style:style>

<style:style style:name="Heading 9"
style:family="paragraph"
style:parent-style-name="Standard" 
style:next-style-name="Standard"
style:class="text">
<style:properties fo:margin-top="0.1665inch"
fo:margin-bottom="0.0417inch" style:font-name="Arial"
fo:font-size="9pt" fo:font-style="italic" fo:font-weight="bold"
style:font-size-asian="9pt" style:font-style-asian="italic"
style:font-weight-asian="bold"/></style:style>

<style:style style:name="List" 
style:family="paragraph"
style:parent-style-name="Text body"
style:class="list">
<style:properties
style:font-name="Palatino"/></style:style>

<style:style style:name="Header" 
style:family="paragraph"
style:parent-style-name="Standard"
style:class="extra">
<style:properties>
<style:tab-stops>
<style:tab-stop
style:position="3inch" style:type="center"/>
<style:tab-stop
style:position="6inch"
style:type="right"/></style:tab-stops></style:properties></style:style>

<style:style style:name="Footer" 
style:family="paragraph"
style:parent-style-name="Standard"
style:class="extra">
<style:properties fo:color="#000000"
style:font-name="AGaramond">
<style:tab-stops>
<style:tab-stop
style:position="3inch" style:type="center"/>
<style:tab-stop
style:position="6inch"
style:type="right"/></style:tab-stops></style:properties></style:style>

<style:style style:name="Table Contents" 
style:family="paragraph"
style:parent-style-name="Text body"
style:class="extra">
<style:properties text:number-lines="false"
text:line-number="0"/></style:style>

<style:style style:name="Table
Heading" style:family="paragraph" 
style:parent-style-name="Table Contents" 
style:class="extra">
<style:properties fo:font-style="italic"
fo:font-weight="bold" style:font-style-asian="italic"
style:font-weight-asian="bold" style:font-style-complex="italic"
style:font-weight-complex="bold" fo:text-align="center"
style:justify-single-word="false" text:number-lines="false"
text:line-number="0"/></style:style>

<style:style style:name="Caption"
style:family="paragraph" 
style:parent-style-name="Standard"
style:class="extra">
<style:properties fo:margin-top="0.0835inch"
fo:margin-bottom="0.0835inch" style:font-name="Palatino"
fo:font-size="10pt" fo:font-style="italic"
style:font-size-asian="10pt" style:font-style-asian="italic"
style:font-size-complex="10pt" style:font-style-complex="italic"
text:number-lines="false"
text:line-number="0"/></style:style>

<style:style style:name="Frame contents" 
style:family="paragraph" 
style:parent-style-name="Text body"
style:class="extra"/>
<style:style style:name="Index"
style:family="paragraph" style:parent-style-name="Standard"
style:class="index">
<style:properties style:font-name="Palatino"
text:number-lines="false"
text:line-number="0"/></style:style>

<style:style style:name=".body"
style:family="paragraph">
<style:properties fo:margin-left="0inch"
fo:margin-right="0inch" fo:margin-top="0inch" fo:margin-bottom="0inch"
fo:color="#000000" style:font-name="AGaramond" fo:font-size="11pt"
fo:language="en" fo:country="US" style:font-name-asian="Times New
Roman" style:font-size-asian="11pt" fo:line-height="0.1937inch"
fo:text-align="justify" style:justify-single-word="false"
fo:orphans="2" fo:widows="2" fo:text-indent="0.25inch"
style:auto-text-indent="false">
<style:tab-stops/></style:properties></style:style>

<style:style style:name=".bullet" 
style:family="paragraph"
style:list-style-name="WW8Num8">
<style:properties
fo:margin-left="0.5inch" fo:margin-right="0inch"
fo:margin-top="0.028inch" fo:margin-bottom="0.028inch"
fo:color="#000000" style:font-name="AGaramond" fo:font-size="11pt"
fo:language="en" fo:country="US" style:font-name-asian="Times New
Roman" style:font-size-asian="11pt" fo:line-height="0.1937inch"
fo:text-align="justify" style:justify-single-word="false"
fo:orphans="0" fo:widows="0" fo:text-indent="0.0008inch"
style:auto-text-indent="false">
<style:tab-stops>
<style:tab-stop
style:position="0inch"/></style:tab-stops></style:properties></style:style>

<style:style style:name=".cell body" 
style:family="paragraph">
<style:properties
fo:color="#000000" style:font-name="Univers 45 Light"
fo:font-size="8pt" fo:language="en" fo:country="US"
style:font-name-asian="Times New Roman" style:font-size-asian="8pt"
style:line-height-at-least="0.139inch" fo:orphans="0"
fo:widows="0"/></style:style>

<style:style style:name=".cell head"
style:family="paragraph">
<style:properties fo:color="#000000"
style:font-name="Univers 45 Light" fo:font-size="8pt" fo:language="en"
fo:country="US" fo:font-weight="bold" style:font-name-asian="Times New
Roman" style:font-size-asian="8pt" style:font-weight-asian="bold"
style:line-height-at-least="0.139inch" fo:orphans="0"
fo:widows="0"/></style:style>

<style:style style:name=".ch"
style:family="paragraph">
<style:properties fo:margin-left="0inch"
fo:margin-right="0inch" fo:margin-top="0.0307inch"
fo:margin-bottom="0inch" fo:color="#000000"
style:font-name="AGaramond" fo:font-size="24pt"
fo:letter-spacing="0.0138inch" fo:language="en" fo:country="US"
fo:font-style="italic" style:font-name-asian="Times New Roman"
style:font-size-asian="24pt" style:font-style-asian="italic"
style:line-height-at-least="0.3335inch" fo:text-align="justify"
style:justify-single-word="false" fo:orphans="0" fo:widows="0"
fo:text-indent="0.8335inch"
style:auto-text-indent="false"/></style:style>

<style:style
style:name=".ch title" 
style:family="paragraph">
<style:properties
fo:margin-top="0.5inch" fo:margin-bottom="0.25inch" fo:color="#000000"
style:font-name="AGaramond" fo:font-size="29.5pt" fo:language="en"
fo:country="US" fo:font-style="italic" style:font-name-asian="Times
New Roman" style:font-size-asian="29.5pt"
style:font-style-asian="italic"
style:line-height-at-least="0.4161inch" fo:orphans="0"
fo:widows="0"/></style:style>

<style:style style:name=".figure"
style:family="paragraph"
style:list-style-name="WW8Num4">
<style:properties
fo:margin-top="0inch" fo:margin-bottom="0inch" fo:color="#000000"
style:font-name="Univers 45 Light" fo:font-size="8pt" fo:language="en"
fo:country="US" fo:font-weight="bold" style:font-name-asian="Times New
Roman" style:font-size-asian="8pt" style:font-weight-asian="bold"
fo:line-height="0.139inch" fo:orphans="0"
fo:widows="0"/></style:style>

<style:style style:name=".head 1"
style:family="paragraph"
style:next-style-name=".body1">
<style:properties
fo:margin-top="0.278inch" fo:margin-bottom="0.1665inch"
fo:color="#000000" style:font-name="AGaramond" fo:font-size="20pt"
fo:language="en" fo:country="US" fo:font-style="italic"
style:font-name-asian="Times New Roman" style:font-size-asian="20pt"
style:font-style-asian="italic" fo:line-height="0.278inch"
fo:orphans="0" fo:widows="0"
fo:keep-with-next="true"/></style:style>

<style:style style:name=".head
2" style:family="paragraph" 
style:parent-style-name=".head 1"
style:next-style-name=".body1">
<style:properties
fo:margin-top="0.0972inch" fo:margin-bottom="0.0972inch"
fo:font-size="16pt" style:font-size-asian="16pt"
fo:line-height="0.222inch"/></style:style>

<style:style style:name=".numlist" 
style:family="paragraph"
style:list-style-name="WW8Num17">
<style:properties
fo:margin-top="0.028inch" fo:margin-bottom="0.028inch"
fo:color="#000000" style:font-name="AGaramond" fo:font-size="11pt"
fo:language="en" fo:country="US" style:font-name-asian="Times New
Roman" style:font-size-asian="11pt" fo:line-height="0.1937inch"
fo:text-align="justify" style:justify-single-word="false"
fo:orphans="0" fo:widows="0">
<style:tab-stops>
<style:tab-stop
style:position="0.5inch"/></style:tab-stops></style:properties></style:style>

<style:style style:name=".reference" 
style:family="paragraph">
<style:properties
fo:margin-left="0.278inch" fo:margin-right="0inch"
fo:margin-top="0.0417inch" fo:margin-bottom="0.0417inch"
fo:color="#000000" style:font-name="AGaramond" fo:font-size="10pt"
fo:language="en" fo:country="US" style:font-name-asian="Times New
Roman" style:font-size-asian="10pt" fo:line-height="0.1665inch"
fo:text-align="justify" style:justify-single-word="false"
fo:orphans="0" fo:widows="0" fo:text-indent="-0.2783inch"
style:auto-text-indent="false">
<style:tab-stops>
<style:tab-stop
style:position="-0.111inch" style:type="right"/>
<style:tab-stop
style:position="0inch"/></style:tab-stops></style:properties></style:style>

<style:style style:name=".table title" 
style:family="paragraph"
style:list-style-name="WW8Num16">
<style:properties fo:color="#000000"
style:font-name="Univers 45 Light" fo:font-size="8pt" fo:language="en"
fo:country="US" fo:font-weight="bold" style:font-name-asian="Times New
Roman" style:font-size-asian="8pt" style:font-weight-asian="bold"
style:line-height-at-least="0.139inch" fo:text-align="justify"
style:justify-single-word="false" fo:orphans="0"
fo:widows="0"/></style:style>

<style:style style:name=".code"
style:family="paragraph">
<style:properties fo:color="#000000"
style:font-name="Courier" fo:font-size="10pt" fo:language="en"
fo:country="US" style:font-name-asian="Times New Roman"
style:font-size-asian="10pt" style:language-asian="en"
style:country-asian="US" style:line-height-at-least="0.1665inch"
fo:orphans="0" fo:widows="0"/></style:style>

<style:style style:name="WW-Document Map" 
style:family="paragraph"
style:parent-style-name="Standard">
<style:properties
style:font-name="Tahoma"
fo:background-color="#000080">
<style:background-image/></style:properties></style:style>

<style:style style:name=".code NOTATION" 
style:family="paragraph">
<style:properties
style:use-window-font-color="true" style:font-name="Helvetica-Narrow"
fo:font-size="9pt" fo:font-weight="bold" style:font-name-asian="Times
New Roman" style:font-size-asian="9pt" style:font-weight-asian="bold"
fo:orphans="2" fo:widows="2"/></style:style>

<style:style style:name=".head 3alone" 
style:family="paragraph"
style:next-style-name=".body">
<style:properties
fo:margin-top="0.0972inch" fo:margin-bottom="0.0555inch"
style:use-window-font-color="true" style:font-name="AGaramond"
fo:font-size="14pt" fo:font-style="italic"
style:font-name-asian="Times New Roman" style:font-size-asian="14pt"
style:font-style-asian="italic" fo:orphans="2"
fo:widows="2"/></style:style>

<style:style style:name=".CALLOUT" 
style:family="paragraph"
style:parent-style-name=".body">
<style:properties
fo:margin-left="0.25inch" fo:margin-right="0.25inch"
fo:margin-top="0.0835inch" fo:margin-bottom="0.0835inch"
fo:text-indent="0inch"
style:auto-text-indent="false"/></style:style>

<style:style style:name=".body1" 
style:family="paragraph"
style:parent-style-name=".body"
style:next-style-name=".body">
<style:properties fo:margin-left="0inch"
fo:margin-right="0inch" fo:font-size="11pt"
style:font-size-asian="11pt" fo:text-indent="0inch"
style:auto-text-indent="false"/></style:style>

<style:style style:name=".code listing" 
style:family="paragraph"
style:next-style-name="Standard"
style:list-style-name="WW8Num5">
<style:properties
style:use-window-font-color="true" style:font-name="Univers 45 Light"
fo:font-size="10pt" fo:font-weight="bold" style:font-name-asian="Times
New Roman" style:font-size-asian="10pt" style:font-weight-asian="bold"
fo:orphans="2" fo:widows="2"/></style:style>

<style:style style:name=".numlist 1" 
style:family="paragraph"
style:parent-style-name=".numlist"
style:next-style-name=".numlist">
<style:properties
fo:margin-left="0inch" fo:margin-right="0inch" fo:text-indent="0inch"
style:auto-text-indent="false"/></style:style>

<style:style style:name="Page Number" 
style:family="text"
style:parent-style-name="WW-Default Paragraph Font"/>

<style:style style:name="Internet link" 
style:family="text"
style:parent-style-name="WW-Default Paragraph Font">
<style:properties
fo:color="#0000ff" style:text-underline="single"
style:text-underline-color="font-color"/></style:style>

<style:style style:name="Strong Emphasis" 
style:family="text"
style:parent-style-name="WW-Default Paragraph Font">
<style:properties
fo:font-weight="bold"
style:font-weight-asian="bold"/></style:style>

<style:style style:name="WW-Default Paragraph Font" 
style:family="text"/>

<style:style style:name="WW8Num2z0" style:family="text">
<style:properties style:font-name="AGaramond
Bold"/></style:style>

<style:style style:name="WW8Num2z1" style:family="text">
<style:properties style:font-name="Helvetica"
fo:font-size="8pt" fo:font-style="normal" fo:font-weight="bold"
style:font-size-asian="8pt" style:font-style-asian="normal"
style:font-weight-asian="bold"/></style:style>

<style:style style:name="WW8Num6z0" style:family="text">
<style:properties
style:font-name="AGaramond Bold"/></style:style>

<style:style style:name="WW8Num7z0" style:family="text">
<style:properties
fo:color="#000000" style:font-name="AGaramond" fo:font-size="20pt"
fo:font-style="italic" style:text-underline="none"
fo:font-weight="normal" style:font-size-asian="20pt"
style:font-style-asian="italic"
style:font-weight-asian="normal"/></style:style>

<style:style style:name="WW8Num8z0" style:family="text">
<style:properties
style:font-name="Wingdings"/></style:style>

<style:style style:name="WW8Num9z0" style:family="text">
<style:properties
fo:color="#000000" style:font-name="AGaramond" fo:font-size="10.5pt"
fo:font-style="normal" style:text-underline="none"
fo:font-weight="normal" style:font-size-asian="10.5pt"
style:font-style-asian="normal"
style:font-weight-asian="normal"/></style:style>

<style:style style:name="WW8Num10z0" style:family="text">
<style:properties
fo:font-weight="bold"
style:font-weight-asian="bold"/></style:style>

<style:style style:name="WW8Num11z0" style:family="text">
<style:properties
style:font-name="AGaramond Bold"/></style:style>

<style:style style:name="WW8Num13z0" style:family="text">
<style:properties
style:font-name="Wingdings"/></style:style>

<style:style style:name="WW8Num15z0" style:family="text">
<style:properties
style:font-name="AGaramond Bold"/></style:style>

<style:style style:name="WW8Num16z0" style:family="text">
<style:properties
style:font-name="AGaramond Bold"/></style:style>

<style:style style:name="Frame" style:family="graphics">
<style:properties
text:anchor-type="paragraph" svg:x="0inch" svg:y="0inch"
style:wrap="parallel" style:number-wrapped-paragraphs="no-limit"
style:wrap-contour="false" style:vertical-pos="top"
style:vertical-rel="paragraph-content" style:horizontal-pos="center"
style:horizontal-rel="paragraph-content"/></style:style>

<style:style style:name="Graphics" style:family="graphics">
<style:properties
text:anchor-type="paragraph" svg:x="0inch" svg:y="0inch"
style:wrap="none" style:vertical-pos="top"
style:vertical-rel="paragraph" style:horizontal-pos="center"
style:horizontal-rel="paragraph"/></style:style>

<style:style style:name="OLE" style:family="graphics">
<style:properties
text:anchor-type="paragraph" svg:x="0inch" svg:y="0inch"
style:wrap="none" style:vertical-pos="top"
style:vertical-rel="paragraph" style:horizontal-pos="center"
style:horizontal-rel="paragraph"/></style:style>

<text:outline-style>
<text:outline-level-style
text:level="1" style:num-format=""/>
<text:outline-level-style
text:level="2" style:num-format=""/>
<text:outline-level-style
text:level="3" style:num-format="1"
text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:outline-level-style>
<text:outline-level-style
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:outline-level-style>
<text:outline-level-style
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:outline-level-style>
<text:outline-level-style
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:outline-level-style>
<text:outline-level-style
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:outline-level-style>
<text:outline-level-style
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:outline-level-style>
<text:outline-level-style
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:outline-level-style>
<text:outline-level-style
text:level="10"
style:num-format=""/>
</text:outline-style>


<text:list-style style:name="WW8Num1">
<text:list-level-style-number text:level="1"
style:num-suffix="." style:num-format="1">
<style:properties
text:space-before="0.25inch"
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num2">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num2z0" style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" text:style-name="WW8Num2z1" style:num-prefix="."
style:num-format="1" text:display-levels="2">
<style:properties
text:min-label-width="0.4inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-format="1"
text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num3">
<text:list-level-style-number text:level="1"
style:num-suffix="." style:num-format="1">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num4">
<text:list-level-style-number text:level="1"
style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-format="1"
text:display-levels="2">
<style:properties
text:min-label-width="0.4inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-format="1"
text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num5">
<text:list-level-style-number text:level="1"
style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-format="1"
text:display-levels="2"/>
<text:list-level-style-number text:level="3"
style:num-format="1" text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num6">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num6z0" style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-format="1"
text:display-levels="2">
<style:properties
text:min-label-width="0.4inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-format="1"
text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num7">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num7z0" style:num-prefix="1." style:num-suffix=" "
style:num-format="1" text:start-value="3">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num8">
<text:list-level-style-bullet text:level="1"
text:style-name="WW8Num8z0" style:num-suffix="."
text:bullet-char="ï§">
<style:properties
text:min-label-width="0.25inch"
style:font-name="Wingdings"/></text:list-level-style-bullet>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num9">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num9z0" style:num-suffix=". "
style:num-format="1">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num10">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num10z0" style:num-suffix="."
style:num-format="1">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num11">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num11z0" style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-format="1"
text:display-levels="2"/>
<text:list-level-style-number text:level="3"
style:num-format="1" text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num12">
<text:list-level-style-number text:level="1"
style:num-suffix=".1" style:num-format="1">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num13">
<text:list-level-style-bullet text:level="1"
text:style-name="WW8Num13z0" style:num-suffix="."
text:bullet-char="ï§">
<style:properties
text:min-label-width="0.25inch"
style:font-name="Wingdings"/></text:list-level-style-bullet>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num14">
<text:list-level-style-number text:level="1"
style:num-suffix="." style:num-format="1">
<style:properties
text:space-before="0.25inch"
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num15">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num15z0" style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-format="1"
text:display-levels="2">
<style:properties
text:min-label-width="0.4inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-format="1"
text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num16">
<text:list-level-style-number text:level="1"
text:style-name="WW8Num16z0" style:num-format="1">
<style:properties
text:min-label-width="0.3inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-format="1"
text:display-levels="2">
<style:properties
text:min-label-width="0.4inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-format="1"
text:display-levels="3">
<style:properties
text:min-label-width="0.5inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-format="1"
text:display-levels="4">
<style:properties
text:min-label-width="0.6inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-format="1"
text:display-levels="5">
<style:properties
text:min-label-width="0.7inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-format="1"
text:display-levels="6">
<style:properties
text:min-label-width="0.8inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-format="1"
text:display-levels="7">
<style:properties
text:min-label-width="0.9inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-format="1"
text:display-levels="8">
<style:properties
text:min-label-width="1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-format="1"
text:display-levels="9">
<style:properties
text:min-label-width="1.1inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num17">
<text:list-level-style-number text:level="1"
style:num-suffix="." style:num-format="1">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:list-style style:name="WW8Num18">
<text:list-level-style-number text:level="1" 
style:num-suffix=".1" style:num-format="1">
<style:properties
text:min-label-width="0.25inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="2" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.1972inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="3" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.3937inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="4" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.5909inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="5" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.7874inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="6" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="0.9846inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="7" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.1815inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="8" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.3787inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="9" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.5752inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number>
<text:list-level-style-number
text:level="10" style:num-suffix="."
style:num-format="1">
<style:properties text:space-before="1.7724inch"
text:min-label-width="0.1965inch"/></text:list-level-style-number></text:list-style>

<text:footnotes-configuration
style:num-format="1" text:start-value="0"
text:footnotes-position="page"
text:start-numbering-at="document"/>

<text:endnotes-configuration
style:num-format="i"
text:start-value="0"/>

<text:linenumbering-configuration
text:number-lines="false" text:offset="0.1965inch"
style:num-format="1" text:number-position="left"
text:increment="5"/>

</office:styles>


<office:automatic-styles>

<style:style style:name="P1" style:family="paragraph" style:parent-style-name="Standard">
<style:properties
fo:margin-left="0inch" fo:margin-right="0.25inch"
style:font-name="AGaramond" fo:text-indent="0inch"
style:auto-text-indent="false"/>
</style:style>

<style:style style:name="P2" style:family="paragraph" style:parent-style-name="Standard">
<style:properties
fo:margin-left="0inch" fo:margin-right="0.25inch"
fo:text-indent="0.25inch"
style:auto-text-indent="false"/>
</style:style>

<style:style style:name="P3" style:family="paragraph" style:parent-style-name="Standard">
<style:properties>
<style:tab-stops>
<style:tab-stop
style:position="0.0626inch"/>
<style:tab-stop
style:position="0.25inch"/>
<style:tab-stop
style:position="2.6252inch"/>
<style:tab-stop
style:position="6.3126inch"/>
<style:tab-stop
style:position="6.3752inch"/>
</style:tab-stops></style:properties></style:style>

<style:style style:name="P4" style:family="paragraph" style:parent-style-name="Standard">
<style:properties
fo:margin-left="0inch" fo:margin-right="0.25inch"
fo:text-indent="0inch"
style:auto-text-indent="false">
<style:tab-stops>
<style:tab-stop
style:position="6.4402inch"
style:type="right"/>
</style:tab-stops></style:properties></style:style>

<style:style style:name="P5" style:family="paragraph" style:parent-style-name="Standard">
<style:properties
fo:margin-left="0inch" fo:margin-right="0.25inch"
fo:text-indent="0inch"
style:auto-text-indent="false">
<style:tab-stops>
<style:tab-stop
style:position="0inch"/>
<style:tab-stop
style:position="2.75inch"/>
<style:tab-stop
style:position="3.1874inch"/>
<style:tab-stop
style:position="6.1874inch"/>
<style:tab-stop
style:position="6.4402inch"
style:type="right"/>
</style:tab-stops></style:properties></style:style>

<style:style style:name="T1" style:family="text" style:parent-style-name="Page Number">
<style:properties
style:font-name="AGaramond"/></style:style>

<style:style style:name="fr1" style:family="graphics" style:parent-style-name="Frame">
<style:properties
style:wrap="parallel" style:number-wrapped-paragraphs="1"
style:vertical-pos="from-top" style:vertical-rel="paragraph-content"
style:horizontal-pos="right" style:horizontal-rel="paragraph-content"
fo:background-color="transparent">
<style:background-image/></style:properties></style:style>

<style:page-master style:name="pm1" style:page-usage="mirrored">
<style:properties
fo:page-width="8.5inch" fo:page-height="11inch" style:num-format="1"
style:print-orientation="portrait" fo:margin-top="0.4925inch"
fo:margin-bottom="0.4925inch" fo:margin-left="1inch"
fo:margin-right="1inch" style:layout-grid-color="#c0c0c0"
style:layout-grid-lines="40"
style:layout-grid-base-height="0.1945inch"
style:layout-grid-ruby-height="0.0555inch"
style:layout-grid-mode="none" style:layout-grid-ruby-below="false"
style:layout-grid-print="false" style:layout-grid-display="false"
style:footnote-max-height="0inch">
<style:footnote-sep
style:width="0.0071inch" style:distance-before-sep="0.0398inch"
style:distance-after-sep="0.0398inch" style:adjustment="left"
style:rel-width="25%"
style:color="#000000"/>
</style:properties>
<style:header-style>
<style:properties
fo:min-height="0.5075inch"/>
</style:header-style>
<style:footer-style>
<style:properties fo:min-height="0inch" fo:margin-top="0.0909inch"/>
</style:footer-style>
</style:page-master>
</office:automatic-styles>


<office:master-styles>
<style:master-page style:name="Standard" style:page-master-name="pm1">

<style:header>
<text:p text:style-name="P1">
<draw:text-box draw:style-name="fr1"
draw:name="Frame1" text:anchor-type="paragraph" svg:y="0.0008inch"
svg:width="0.2429inch" svg:height="0.0161inch" draw:z-index="2">
<text:p text:style-name="Header">
<text:span text:style-name="T1">
<text:page-number text:select-page="current">10
</text:page-number></text:span></text:p></draw:text-box></text:p>
</style:header>

<style:header-left>
<text:p text:style-name="P2">
<draw:text-box draw:style-name="fr1"
draw:name="Frame2" text:anchor-type="paragraph" svg:y="0.0008inch"
svg:width="0.2429inch" svg:height="0.0161inch" draw:z-index="3">
<text:p text:style-name="Standard">
<text:page-number text:select-page="current">10
</text:page-number></text:p></draw:text-box></text:p>
</style:header-left>

<style:footer>
<text:p text:style-name="P3">
<text:tab-stop/></text:p>
<text:p text:style-name="P3">
Author Template 
<text:tab-stop/>
Manning Publications Co.
<text:tab-stop/>
<text:span text:style-name="T1">
<text:page-number text:select-page="current">10
</text:page-number></text:span></text:p>
</style:footer>

<style:footer-left>
<text:p text:style-name="P4"/>
<text:p text:style-name="P5">
Author Template
<text:tab-stop/>
Manning Publications Co.
<text:tab-stop/>
<text:tab-stop/>
<text:span text:style-name="T1">
<text:page-number text:select-page="current">10
</text:page-number></text:span></text:p>
</style:footer-left>

</style:master-page>
</office:master-styles>
</office:document-styles>

'''
