"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.3
"""

from   docutils.languages.en             import bibliographic_fields
from   docutils.parsers.rst.languages.en import directives
from   docutils.core                     import publish
from   re                                import findall
import sys
from   wxPython.wx                       import wxLogMessage

def get_rest_bibl_fields():
    # suitable for autocompletion in wxStyledTextCtrl
    bibl_fields = ''
    list = bibliographic_fields.keys()
    list.sort()
    for value in list:
        bibl_fields = '%s %s:' % (bibl_fields, value)
    return bibl_fields[1:]

def get_rest_directives():
    # suitable for autocompletion in wxStyledTextCtrl
    drvs = ''
    list = directives.keys()
    list.sort()
    for value in list:
        drvs = '%s %s::' % (drvs, value)
    return drvs[1:]

def rest2html(file, htmlfile, stylesheet):
    # publish reST as HTML and return
    # linenumbers of warnings and errors
    warning_lines = []
    error_lines = []
    sys.stderr = StdCatcher()
    publish(writer_name='html',
            argv = ['--stylesheet', stylesheet,
                    '-g', '--toc-top-backlinks',
                    '-o', 'ISO-8859-1', file, htmlfile]
           )
    warning_lines = sys.stderr.GetWarnings()
    error_lines = sys.stderr.GetErrors()
    sys.stderr = sys.__stderr__
    return warning_lines, error_lines
    
class StdCatcher:
    # suitable for catching docutils stderr-output
    def __init__(self):
        self.warnings = []
        self.errors = []
        self.text = ''
        
    def write(self,s):
        if s.startswith('WARNING'):
            for i in findall('line \d+', s):
                self.warnings.append(int(i.split(' ')[1])-1)
        if s.startswith('ERROR') or s.startswith('SEVERE'):
            for i in findall('line \d+', s):
                self.errors.append(int(i.split(' ')[1])-1)
        wxLogMessage(s)

    def GetWarnings(self):
        self.warnings.sort()
        return self.warnings

    def GetErrors(self):
        self.errors.sort()
        return self.errors

