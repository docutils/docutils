# Author: Gunnar Schwant
# Contact: g.schwant@gmx.de
# Revision: $Revision$
# Date: $Date$
# Copyright: This module has been placed in the public domain.

from docutils import writers, nodes, languages
import docutils, html4css1, os

class Writer(html4css1.Writer):

    def __init__(self):
        writers.Writer.__init__(self)
        self.translator_class = HTMLTranslator

class HTMLTranslator(html4css1.HTMLTranslator):

    def __init__(self, document):
        html4css1.HTMLTranslator.__init__(self, document)
        self.navigation_bgcolor = '#FFFFFF'
        self.body_prefix = [self.get_body_prefix()]
        self.body_suffix = [self.get_body_suffix()]

    def get_body_prefix(self):
        navtop = os.path.join(os.path.split(self.settings._destination)[0],
                              'top.nav')
        navright = os.path.join(os.path.split(self.settings._destination)[0],
                                'right.nav')
        if os.path.exists(navright):
            colspan = 3
            width = '70%'
        else:
            colspan = 2
            width = '85%'
        navleft = '%s%s' % (os.path.splitext(self.settings._destination)[0],
                            '.nav')
        bgcolor = color1 = color2 = self.navigation_bgcolor
        cornerpic = None
        if not os.path.exists(navleft):
            navleft = os.path.join(os.path.split(self.settings._destination)[0],
                                   'left.nav')
        if os.path.exists(navleft):
            if os.path.exists(navtop):
                f = open(navtop, 'rt')
                lines = f.readlines()
                f.close()
                for line in lines:
                    val = line.split('|')
                    for i in range(len(val)):
                        val[i] = val[i].strip()
                    if val[0] == 'color':
                        bgcolor = color1 = color2 = val[1]
                        self.navigation_bgcolor = bgcolor
                    if val[0] == 'cornerpic':
                        cornerpic = val[1]
            body_prefix = '<style type="text/css"><!--\n' \
                          'a.navigation, a.navigation-top { ' \
                          'text-decoration: none ; color: #000000 }\n' \
                          'a.navigation-top { font-weight: bold }\n' \
                          'a.navigation:hover, a.navigation-top:hover { ' \
                          'text-decoration: underline }\n' \
                          'table.navigation { font-size: 10pt ; ' \
                          'border-collapse: collapse ;  border-width: 0pt ' \
                          '; border-color: #FFFFFF }\n' \
                          'td.navigation { font-size: 8pt ; padding-left: ' \
                          '0em ; padding-right: 0em }\n' \
                          '--></style>\n</head>\n<body>\n<table width="100%" ' \
                          'class="navigation" border="0" cellspacing="0" ' \
                          'cellpadding="0">'
            if os.path.exists(navtop):
                body_prefix = body_prefix + '\n<tr><td width="15%" '
                body_prefix = '%s class="navigation" bgcolor="%s">' \
                              % (body_prefix, bgcolor)
                if cornerpic != None:
                    body_prefix = '%s\n<center><img src="%s"></center>' \
                                  % (body_prefix, cornerpic)
                body_prefix = '%s\n</td><td bgcolor="%s" ' \
                              'class="navigation">&nbsp;&nbsp;</td>' \
                              '<td bgcolor="%s" colspan="%d" ' \
                              'class="navigation">' % (body_prefix,
                                                       bgcolor, bgcolor,
                                                       colspan)
                body_prefix = body_prefix + '\n<table class="navigation" ' \
                              'width="100%" border="0" cellspacing="0" ' \
                              'cellpadding="0">'
                body_prefix = '%s\n<tr align="left">' % body_prefix
                for line in lines:
                    val = line.split('|')
                    for i in range(len(val)):
                        val[i] = val[i].strip()
                    if val[0] == 'link':
                        body_prefix = '%s\n<td class="navigation"><ul><li>' \
                                      '<a href="%s" class="navigation-top">' \
                                      '%s</a></li></ul></td>' \
                                      % (body_prefix, val[2], val[1])
                    elif val[0] == 'raw':
                        body_prefix = '%s\n<td class="navigation">%s</td>' \
                                      % (body_prefix, val[1])
                body_prefix = '%s\n</tr></table></td></tr>' % body_prefix
            f = open(navleft, 'rt')
            lines = f.readlines()
            f.close()
            body_prefix = '%s\n<tr><td class="navigation" bgcolor="%s" ' \
                          % (body_prefix, bgcolor)
            body_prefix = body_prefix + ' width="15%" valign="top">'
            body_prefix = body_prefix + '\n<table class="navigation" ' \
                          'width="100%" border="0" cellspacing="0" ' \
                          'cellpadding="3">'
            for line in lines:
                val = line.split('|')
                for i in range(len(val)):
                    val[i] = val[i].strip()
                if val[0] == 'colors':
                    (color1, color2) = (val[1], val[2])
                elif val[0] == 'link':
                    body_prefix = '%s\n<tr><td bgcolor="%s" ' \
                                  'class="navigation">&nbsp;<a href="%s" ' \
                                  'class="navigation">%s</a></td></tr>' \
                                  % (body_prefix, color2, val[2], val[1])
                elif val[0] == 'section':
                    body_prefix = '%s\n<tr><td bgcolor="%s" ' \
                                  'class="navigation">&nbsp;<b>' \
                                  % (body_prefix, color1)
                    if color1 != bgcolor:
                        body_prefix = '%s<font color="#FFFFFF">%s</font>' \
                                      '</b></td></tr>' \
                                      % (body_prefix, val[1])
                    else:
                        body_prefix = '%s%s</b></td></tr>' \
                                      % (body_prefix, val[1])
                elif val[0] == 'raw':
                    body_prefix = '%s\n<tr><td bgcolor="%s" ' \
                                  'class="navigation">&nbsp;%s</td></tr>' \
                                  % (body_prefix, color2, val[1])
            body_prefix = '%s\n</table>\n</td>\n<td ' \
                          'width="15">&nbsp;&nbsp;</td>\n<td width="%s">' \
                          % (body_prefix, width)
        else:
            body_prefix = '</head>\n<body>\n'
        return body_prefix

    def get_body_suffix(self):
        bgcolor = color1 = color2 = self.navigation_bgcolor
        navleft = '%s%s' % (os.path.splitext(self.settings._destination)[0],
                            '.nav')
        if not os.path.exists(navleft):
            navleft = os.path.join(os.path.split(self.settings._destination)[0],
                                   'left.nav')
        navright = os.path.join(os.path.split(self.settings._destination)[0],
                                'right.nav')
        if os.path.exists(navleft) and os.path.exists(navright):
            f = open(navright, 'rt')
            lines = f.readlines()
            f.close()
            body_suffix = '</td><td width="15">&nbsp;&nbsp;</td>\n<td ' \
                          'class="navigation" width="15%" valign="top" '
            body_suffix = '%s bgcolor="%s">' % (body_suffix, bgcolor)
            body_suffix = body_suffix + '\n<table class="navigation" ' \
                          'width="100%" border="0" cellspacing="0" ' \
                          'cellpadding="3">' 
            for line in lines:
                val = line.split('|')
                for i in range(len(val)):
                    val[i] = val[i].strip()
                if val[0] == 'link':
                    body_suffix = '%s\n<tr><td bgcolor="%s" ' \
                                  'class="navigation">&nbsp;<a href="%s" ' \
                                  'class="navigation">%s</a></td></tr>' \
                                  % (body_suffix, color2, val[2], val[1])
                elif val[0] == 'colors':
                    (color1, color2) = (val[1], val[2])
                elif val[0] == 'section':
                    body_suffix = '%s\n<tr><td bgcolor="%s" ' \
                                  'class="navigation">&nbsp;<b>' \
                                  % (body_suffix, color1)
                    if color1 != bgcolor:
                        body_suffix = '%s<font color="#FFFFFF">%s</font>' \
                                      '</b></td></tr>' \
                                      % (body_suffix, val[1])
                    else:
                        body_suffix = '%s%s</b></td></tr>' % (body_suffix,
                                                              val[1])
                elif val[0] == 'raw':
                    body_suffix = '%s\n<tr><td bgcolor="%s" ' \
                                  'class="navigation">&nbsp;%s</td></tr>' \
                                  % (body_suffix, color2, val[1])
            body_suffix = '%s\n</table>\n</td></tr></table>\n</head>\n' \
                          '</body>\n' % body_suffix
        elif os.path.exists(navleft):
            body_suffix = '</td></tr></table>\n</head>\n</body>\n'
        else:
            body_suffix = '</head>\n</body>\n'
        return body_suffix
