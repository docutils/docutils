import os, sys
fs = 'table'
ss = '-block-container'
s="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""


sys.stdout.write(s)

s = '\n\n\n'
fs = 'table'
ss = ''
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'thead'
ss = '-header'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'thead'
ss = '-cell'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'thead'
ss = '-block'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'table'
ss = '-body'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'table'
ss = '-header-row'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'table'
ss = '-row'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'table'
ss = '-cell'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)

s = '\n\n\n'
fs = 'cell'
ss = '-block'
s+="""    <xsl:template match= "xsl:attribute-set[@name='%s1%s']|\n""" % (fs, ss)
for n in range(2,31):
    s += """     xsl:attribute-set[@name='%s%s%s']|\n""" % (fs, n, ss)
s = s[:-2]
s += '"'
s += """ priority="3"/>"""
sys.stdout.write(s)
