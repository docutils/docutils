"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.2.2
"""

from   docutils.languages.en             import bibliographic_fields
from   docutils.parsers.rst.languages.en import directives
from   docutils.core                     import publish_cmdline
from   docutils                          import __version__
from   re                                import findall
import os, sys
from   wxPython.wx                       import wxLogMessage

docutilsversion = __version__

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

def rest2html(file, htmlfile, dir):
    # publish reST as HTML
    current_dir = os.path.abspath(os.curdir)
    sys.stderr = StdCatcher()
    os.chdir(dir)    
    publish_cmdline(writer_name='html', argv = [file, htmlfile])
    os.chdir(current_dir)
    sys.stderr = sys.__stderr__
    return 1

class StdCatcher:
    # suitable for catching docutils stderr-output
    def __init__(self):
        self.text = ''
        
    def write(self,s):
        wxLogMessage(s)

def get_errors(str):
    errors = []
    warnings = []
    lines = str.splitlines()
    for s in lines:
        if docutilsversion < '0.2.5':
            if s.startswith('WARNING'):
                for i in findall('line \d+', s):
                    warnings.append(int(i.split(' ')[1])-1)
            if s.startswith('ERROR') or s.startswith('SEVERE'):
                for i in findall('line \d+', s):
                    errors.append(int(i.split(' ')[1])-1)
        else:
            parts = s.split(': (')
            if len(parts) > 1:
                s2 = parts[1]
                if s2.startswith('WARNING'):
                    for i in findall(':\d+:', s):
                        warnings.append(int(i[1:-1])-1)
                if s2.startswith('ERROR') or s2.startswith('SEVERE'):
                    for i in findall(':\d+:', s):
                        errors.append(int(i[1:-1])-1)
    errors.sort()
    warnings.sort()
    return warnings, errors
