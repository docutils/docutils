#!/Library/Frameworks/Python.framework/Versions/2.7/Resources/Python.app/Contents/MacOS/Python
# $Id$ 

import cStringIO, sys
from lxml import etree
import lxml
import os


the_settings_dict = {   '_config_files': ['/Users/cynthia/.docutils'],
    '_destination': None,
    '_disable_config': None,
    '_source': 'test.rst',
    'auto_id_prefix': 'id',
    'config': None,
    'datestamp': None,
    'debug': None,
    'docinfo_xform': 1,
    'doctitle_xform': 1,
    'doctype_declaration': 1,
    'dump_internals': None,
    'dump_pseudo_xml': None,
    'dump_settings': None,
    'dump_transforms': None,
    'error_encoding': 'US-ASCII',
    'error_encoding_error_handler': 'backslashreplace',
    'exit_status_level': 5,
    'expose_internals': None,
    'file_insertion_enabled': 1,
    'footnote_backlinks': 1,
    'generator': None,
    'halt_level': 4,
    'id_prefix': '',
    'indents': None,
    'input_encoding': None,
    'input_encoding_error_handler': 'strict',
    'language_code': 'en',
    'newlines': None,
    'output_encoding': 'utf-8',
    'output_encoding_error_handler': 'xmlcharrefreplace',
    'pep_base_url': 'http://www.python.org/dev/peps/',
    'pep_file_url_template': 'pep-%04d',
    'pep_references': None,
    'raw_enabled': 1,
    'report_level': 2,
    'rfc_base_url': 'http://www.faqs.org/rfcs/',
    'rfc_references': None,
    'sectnum_xform': 1,
    'sectsubtitle_xform': 0,
    'source_link': None,
    'source_url': None,
    'strict_visitor': None,
    'strip_classes': None,
    'strip_comments': None,
    'strip_elements_with_classes': None,
    'tab_width': 8,
    'title': 'BOZO',
    'toc_backlinks': 'entry',
    'traceback': None,
    'trim_footnote_reference_space': None,
    'warning_stream': None,
    'xml_declaration': 1}

def publish_xml_cmdline (in_path = None, in_string = None, out_path = None, settings_overrides=None ):
    try:
        import locale
        locale.setlocale(locale.LC_ALL, '')
    except:
        pass


    from docutils.core import  default_description, default_usage, publish_file, publish_string
    import docutils.core
    description = ('Generates Docutils-native XML from standalone '
               'reStructuredText sources.  ' + default_description)

    if not in_path and not in_string:
        raise TypeError('publish_xml_cmdlind() must have either "in_path" or "in_string" as parameters')


    if in_path:
        out_string = publish_file(source_path= in_path, destination_path=out_path, 
                    settings_overrides = settings_overrides, writer_name='xml')

    elif in_string:
        out_string = publish_string(source= in_string, destination_path=out_path, 
                    settings_overrides = settings_overrides, writer_name='xml')
    return out_string


def report_xsl_error(transform_error_obj):
    for error_obj in transform_error_obj:
        sys.stderr.write(error_obj.message)
        sys.stderr.write('\n')
        if error_obj.line != 0 and error_obj.column != 0:
            sys.stderr.write(str(error_obj.line))
            sys.stderr.write(str(error_obj.column))
        #print error_obj.type, 'type'
        #print error_obj.type_name, 'type_name'

def validate_docutils(xml_file):
    doc = etree.parse(file(xml_file))
    validate_docutils_rng(doc)

# validate through xslt stylesheet
def validate_fo_xsl(xml_file):
    xsl_ss = os.path.join(os.path.dirname(__file__), 'valid','folint.xsl')
    xslt_doc = etree.parse(xsl_ss) 
    transform = etree.XSLT(xslt_doc)
    try:
        indoc = etree.parse(xml_file) 
    except lxml.etree.XMLSyntaxError, msg:
        sys.stderr.write('Invalid XML\n')
        sys.stderr.write(str(msg))
        sys.stderr.write('\n')
        return 1
    try:
        outdoc = transform(indoc)
    except lxml.etree.XSLTApplyError, error:
        msg = 'error converting %s to %s with %s:\n' % (xml_file, out_file, xslt_file)
        msg += str(error)
        msg += '\n'
        report_xsl_error(transform.error_log)
        return 1
    report_xsl_error(transform.error_log)
    return  len(transform.error_log)

def validate_docutils_rng(xml_obj):
    the_rng = os.path.join(os.path.dirname(__file__), 'valid','docutils.rng') 
    relaxng_doc = etree.parse(file(the_rng))
    relaxng = etree.RelaxNG(relaxng_doc)
    is_valid = relaxng.validate(xml_obj)
    if not is_valid:
        sys.stderr.write('Document not Valid:\n')
        report_xsl_error(relaxng.error_log)
        return 1



def validate_docutils_dtd(xml_obj):
    the_dtd = os.path.join(os.path.dirname(__file__), 'valid','docutils.dtd') 
    if not os.path.isfile(the_dtd):
        msg = '"%s" cannot be found\n' % (the_dtd)
        raise IOError(msg)
    
    dtd = etree.DTD(file(the_dtd))
    is_valid = dtd.validate(xml_obj)
    if not is_valid:
        sys.stderr.write('Document not Valid:\n')
        report_xsl_error(dtd.error_log)
        return 1

def transform_lxml(xslt_file, xml_file, valid_docutils = True, 
        param_dict = {}, out_file = None, verbose = 0, valid_fo = True):
    # have to put quotes around string params
    temp = {}
    the_keys = param_dict.keys()
    for the_key in the_keys:
        if len(the_key) > 0: 
            if the_key[0] == '"' and the_key[-1] == '"':
                temp[the_key] = param_dict[the_key]
            elif the_key[0] == "'" and the_key[-1] == "'":
                temp[the_key] = param_dict[the_key]
            else:
                temp[the_key] = "'%s'" % param_dict[the_key]
    param_dict = {}
    param_dict.update(temp)

    xslt_doc = etree.parse(xslt_file)
    try:
        transform = etree.XSLT(xslt_doc)
    except lxml.etree.XSLTParseError, error:
        sys.stderr.write(str(error) + '\n')
        return 1
    try:
        indoc = etree.parse(xml_file)
    except lxml.etree.XMLSyntaxError, msg:
        sys.stderr.write('Invalid XML\n')
        sys.stderr.write(str(msg))
        sys.stderr.write('\n')
        return 1
    if valid_docutils:
        not_valid = validate_docutils_rng(indoc)
        if not_valid:
            return 1
        elif verbose > 2:
            sys.stderr.write('docutils document is valid\n')
    try:
        outdoc = transform(indoc, **param_dict)
    except lxml.etree.XSLTApplyError, error:
        msg = 'error converting %s to %s with %s:\n' % (xml_file, out_file, xslt_file)
        msg += str(error)
        msg += '\n'
        report_xsl_error(transform.error_log)
        return 1
    report_xsl_error(transform.error_log)
    if valid_fo:
        not_valid = validate_fo_xsl(cStringIO.StringIO(str(outdoc)))
        if not_valid:
            return 1
        elif verbose > 2:
            sys.stderr.write('FO document is valid\n')
    if out_file:
        write_obj = open(out_file, 'w')
        outdoc.write(write_obj)
        write_obj.close()
    else:
        sys.stdout.write(str(outdoc))


if __name__ == '__main__':
    custom = {'title':'Doc Title'}
    publish_xml_cmdline('test.rst', 'out.xml', custom)
