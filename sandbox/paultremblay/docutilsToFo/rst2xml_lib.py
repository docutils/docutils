#!/Library/Frameworks/Python.framework/Versions/2.7/Resources/Python.app/Contents/MacOS/Python
# $Id$ 

import cStringIO, sys


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

def publish_xml_cmdline (in_path = None, out_path = None, settings_overrides=None):
    try:
        import locale
        locale.setlocale(locale.LC_ALL, '')
    except:
        pass


    from docutils.core import  default_description, default_usage
    import docutils.core
    description = ('Generates Docutils-native XML from standalone '
               'reStructuredText sources.  ' + default_description)

    if out_path:
        sys.stdout = file(out_path, 'w')
    if in_path:
        sys.stdin = file(in_path)

    pub = docutils.core.Publisher(None, None, None, settings=None)
    pub.set_components('standalone', 'restructuredtext', 'xml')
    output = pub.publish(
        None, default_usage, description, None, settings_overrides,
        config_section=None, enable_exit_status=1)
    return output

if __name__ == '__main__':
    custom = {'title':'Doc Title'}
    publish_xml_cmdline('test.rst', 'out.xml', custom)
