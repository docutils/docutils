"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.3
"""

from   docutils.languages                import en, de, fr, it
from   docutils.parsers.rst.languages.en import directives
from   docutils.core                     import publish_file
from   docutils                          import __version__
from   re                                import findall
import os, sys
from   wxPython.wx                       import wxLogMessage

docutilsversion = __version__

language_codes = ['en', 'de', 'fr', 'it']

publishers = { 'HTML': (None, 'html', '.html'),
               'Docutils-XML': (None, 'xml', '.xml'),
               'LaTeX': (None, 'latex', '.tex'),
               'PEP-HTML': ('pep', 'pep_html', '.html'),
               'Pseudo-XML': (None, None, '.xml'),
               }

try:
    from docutils.writers import htmlnav
    publishers['HTML with navigation-bars'] = (None, 'htmlnav', '.html')
except:
    pass

def get_rest_bibl_fields(language_code='en'):
    # suitable for autocompletion in wxStyledTextCtrl
    bibl_fields = ''
    if language_code not in language_codes:
        language_code = 'en'
    if language_code == 'en':
        list = en.bibliographic_fields.keys()
    if language_code == 'de':
        list = de.bibliographic_fields.keys()
    if language_code == 'fr':
        list = fr.bibliographic_fields.keys()
    if language_code == 'it':
        list = it.bibliographic_fields.keys()
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

def publish_document(writer, infile, outfile, outdir):
    current_dir = os.path.abspath(os.curdir)
    sys.stderr = StdCatcher()
    os.chdir(outdir)
    if publishers[writer][0] != None:
        publish_file(reader_name=publishers[writer][0],
                     writer_name=publishers[writer][1],
                     source_path=infile,
                     destination_path=os.path.join(outdir,outfile))
    else:
        if publishers[writer][1] != None:
            publish_file(writer_name=publishers[writer][1],
                         source_path=infile,
                         destination_path=os.path.join(outdir,outfile))
        else:
            publish_file(source_path=infile,
                         destination_path=os.path.join(outdir,outfile))
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
