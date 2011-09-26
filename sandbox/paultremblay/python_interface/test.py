import sys, os, glob, commands, shlex, subprocess, argparse
import docutilsToFo.rst2xml_lib
from lxml import etree
import lxml
from docutilsToFo.rst2xml_lib import transform_lxml, publish_xml_cmdline, validate_fo_xsl 

# $Id$ 
TEST = False
STRICT = True

parser = argparse.ArgumentParser(description='Test all the files') 
parser.add_argument('--xslt', nargs=1, choices = ['xsltproc', 'lxml', 'saxon', 'xalan'], default=['lxml'], 
        dest='xsl_transform', help = 'choose which processer to use when transforming' ) 
parser.add_argument('--pdf', action="store_const", const=True, help = 'convert to PDF', dest='pdf')
parser.add_argument('--no-rst', action="store_const", const=True, help = 'don\'t convert to RST', dest='no_rst')
parser.add_argument('--no-fo', action="store_const", const=True, help = 'don\'t convert to FO', dest='no_fo')
parser.add_argument('--strict', nargs=1, choices = ['True', 'False'],  
        default = 'True', help = 'whether to quit on errors', dest='strict')
parser.add_argument('--debug', action="store_const", const=True, help = 'do debug messaging', dest='debug')
parser.add_argument('--verbose', action="store_const", const=True, help = 'be verose in messaging', dest='verbose')
parser.add_argument('--clean', action="store_const", const=True, help = 'whether to remove files after test', dest='clean')
parser.add_argument('--full', action="store_const", const=True, help = 'Do a full test (for developer only)', dest='full')

arg = parser.parse_args()
PDF = arg.pdf
if  arg.strict == 'True':
    STRICT = True
else:
    STRICT = False
debug = False
debug = arg.debug
verbose = arg.verbose
if debug: verbose = True
FULL = False
if arg.full:
    FULL = True
    PDF = True

# =========================================================================================
# stylesheet  test
# =========================================================================================
test_dict = {
        'long_plain.xml':[({'page-layout':'simple'}, 'simple_no_page_nos.fo'), 
                ({'page-layout': 'first'}, 'first_page_diff_no_page_nos.fo'),  
                ({'page-layout': 'odd-even'}, 'odd_even_no_page_nos.fo'),
                ({'page-layout': 'first-odd-even'}, 'first_odd_even_no_page_nos.fo')
                ],
        'header_footer.xml':[({'page-layout': 'simple'}, 'header_footer.fo'),
                ({'page-layout': 'simple', 'suppress-first-page-header': 'True'}, 'header_footer2.fo'),
                ({'page-layout': 'first' }, 'first_page_header_footer.fo'),
                ({'page-layout': 'first', 'suppress-first-page-header': 'True'}, 'first_page_suppress_header.fo'),
                ({'page-layout': 'first', 'suppress-first-page-header': 'True', 'suppress-first-page-header': 'True'},
                    'first_suppress_header_footer.fo'),
                ({'page-layout': 'odd-even' }, 'odd_even_page_header_footer.fo'),
                ({'page-layout': 'first-odd-even' }, 'first-odd_even_page_header_footer.fo'),
                ({'page-layout': 'first-odd-even', 'suppress-first-page-header': 'True', 'suppress-first-page-header': 'True'},
                    'first_odd_even_page_header_footer_suppress_first.fo'),
                ],
        'opt_list.xml':[({},'opt_list.fo'),
                ({'option-list-format': 'definition'}, 'opt_list_as_def.fo') 
                ],
        'table_csv.xml':[({},'table_csv.fo'),
                ({'table-title-placement':'top'},'table_caption_top_csv.fo')
                ],
        'footnotes.xml':[({},'footnotes.fo'),
                ({'footnote-style': 'traditional'},'footnotes_traditional.fo')
                ],
        'endnotes.xml':[({'footnote-placement':'endnote'}, 'endnotes.fo')
                ],
        'hyperlinks.xml':[({}, 'hyperlinks.fo'),
                ({'internal-link-type':'page'}, 'hyperlinks_page.fo'),
                ({'internal-link-type':'page-link'}, 'hyperlinks_link_page.fo')
                ],

        'title_subtitle.xml':[({}, 'title_subtitle.fo'),
                ({'page-layout': 'first'}, 'title_subtitle_first.fo'),
                ({'page-layout': 'odd-even'}, 'title_subtitle_odd_even.fo'),
                ({'page-layout': 'first-odd-even'}, 'title_subtitle_first_odd_even.fo')
                ],

        'bibliographic_fields_toc.xml':[
                ({},'bibliographic_fields_toc.fo'),
                ({'page-layout':'first'},'bibliographic_fields_first_toc.fo'),
                ({'page-layout':'odd-even'},'bibliographic_fields_odd_even_toc.fo'),
                ({'page-layout':'first-odd-even'},'bibliographic_fields_first_odd_even_toc.fo'),
                ],
        'front_body.xml':[({},'front_matter.fo'),
                    ({'title-pagination':'with-front',
                    'bibliographic-pagination':'with-front',
                    'dedication-pagination':'with-front',
                    'dedication-pagination':'with-front',
                    'abstract-pagination':'with-front',
                    'toc-pagination':'with-front'},
                    'front_matter2.fo'),
                    ({'title-pagination':'with-front',
                    'bibliographic-pagination':'with-front',
                    'dedication-pagination':'with-body',
                    'dedication-pagination':'with-front',
                    'abstract-pagination':'with-toc',
                    'toc-pagination':'with-front'},
                    'front_matter3.fo'),
                    ({'title-pagination':'with-body',
                    'bibliographic-pagination':'with-front',
                    'dedication-pagination':'with-toc',
                    'dedication-pagination':'with-front',
                    'abstract-pagination':'with-toc',
                    'toc-pagination':'with-body'},
                    'front_matter4.fo'),
                    ({'title-pagination':'with-body',
                    'bibliographic-pagination':'with-front',
                    'dedication-pagination':'with-toc',
                    'dedication-pagination':'with-front',
                    'abstract-pagination':'with-toc',
                    'toc-pagination':'with-body',
                    'front-order':'toc, abstract, dedication,,title, bibliographic'},
                    'front_matter5.fo'),
            ],

        'table_borderless.xml':[({}, 'table_borderless.fo'),],
        'table_grid_class.xml':[({}, 'table_grid_class.fo'),],
        'table_long1.xml':[({}, 'table_long1.fo'),
                ({'table-title-placement':'top'}, 'table_long1_top_title.fo'),
                ({'table-title-placement':'top','long-rows-first-page':'8' }, 'table_long1_top_title_split.fo'),

                ],
        'table_long2.xml':[({}, 'table_long2.fo'),
                ({'table-title-placement':'top'}, 'table_long2_top_title.fo'),
                ({'table-title-placement':'top','long-rows-first-page':'8' }, 'table_long2_top_title_split.fo'),
                ],
        }
# =========================================================================================
# Docutils_to_fo test
# =========================================================================================

docfo_commands = [
        ('long_plain.xml', 'paper_size.conf'),
        ('long_plain.xml', 'margins_simple.conf'),
        ('long_plain.xml', 'margins_first_odd_even.conf'),
        ('long_plain.xml', 'header_footer2.conf'),
        ('long_plain.xml', 'header_footer3.conf'),
        ('simple.xml', 'paragraph1.conf'),
        ('simple.xml', 'paragraph2.conf'),
        ('simple.xml', 'paragraph3.conf'),
        ('simple2.xml', 'paragraph4.conf'), # long para should not break across page
        ('simple.xml', 'paragraph5.conf'), # first paragraph test
        ('front_body.xml', 'front1.conf'), # all front matter
        ('front_body.xml', 'front2.conf'), # change order of front matter
        ('title_subtitle.xml', 'title1.conf'), # title with front matter
        ('bibliographic_fields.xml', 'bibliographic_fields.conf'), # generic bibliographic fields
        ('bibliographic_fields.xml', 'bibliographic_fields2.conf'), # bibliographic fields as blocks
        ('bibliographic_fields.xml', 'dedication1.conf'), # generic dedication
        ('bibliographic_fields.xml', 'abstract1.conf'), # generic dedication
        ('toc.xml', 'toc1.conf'), # generic toc
        ('bibliographic_fields_toc.xml', 'page_number1.conf'), # page numbering
        ('section.xml', 'section1.conf'), # generic sections
        ('transition.xml', 'transition1.conf'), # transition
        ('bullet_list.xml', 'bullet_list1.conf'), # bullet list
        ('enumerated_list.xml', 'enumerated_list1.conf'), # enumerated list
        ('definition_list.xml', 'definition_list1.conf'), # definition list
        ('field_lists.xml', 'field_list1.conf'), # field list
        ('opt_list.xml', 'option_list1.conf'), # option list as list
        ('opt_list.xml', 'option_list2.conf'), # option list as definition list
        ('literal_block.xml', 'literal_block1.conf'), # literal block
        ('line_block.xml', 'line_block1.conf'), # line block
        ('header_footer2.xml', 'header_footer4.conf'), # first header and footer
        ('header_footer5.xml', 'header_footer5.conf'), # odd even
        ('header_footer5.xml', 'header_footer6.conf'), # odd even, suppress first
        ('header_footer_toc1.xml', 'header_footer_toc1.conf'), # first and other
        ('header_footer_toc4.xml', 'header_footer_toc2.conf'), # odd even, 
        ('header_footer_toc3.xml', 'header_footer_toc3.conf'), # first odd even, 
        ('header_footer_toc2.xml', 'header_footer_toc1.conf'), #  suppress first
        ('block.xml', 'block_quote1.conf'), #  block quote
        ('table_simple.xml', 'table1.conf'), #  default table
    ]

def error_func(msg, the_path = None):
    if the_path:
        sys.stderr.write('Problems with "%s"\n' % the_path)
    if type(msg) == type(''):
        sys.stderr.write(msg)
    if STRICT:
        sys.exit(1)
        pass

def transform_xsl(xsl_file, xml_file, param_dict = {}, out_file = None, xsl_transform = 'lxml'):
    if not out_file:
        base, ext = os.path.splitext(xml_file)
        out_file = '%s.fo' % (base)
    if xsl_transform == 'xsltproc':
        command = 'xsltproc -o %s ' % (out_file)
        params = param_dict.keys()
        for param in params:
            command += ' --stringparam %s "%s" ' % (param, param_dict[param])
        command += ' %s %s ' % (xsl_file, xml_file)
        if TEST:
            print command
        status, output = commands.getstatusoutput(command)
        if status:
            msg = 'error converting %s to FO\n' % (xml_file)
            msg += 'command = "%s" \n' % (command)
            msg += output
            msg += '\n'
            error_func(msg)
    elif xsl_transform == 'saxon':
        command = 'saxon.sh '
        command += ' -o %s' % (out_file)
        if STRICT:
            command += ' -warnings:fatal '
        command  += ' %s %s' % (xml_file, xsl_file)
        params = param_dict.keys()
        for param in params:
            command += ' %s="%s" ' % (param, param_dict[param])
        status, output = commands.getstatusoutput(command)
        if status:
            msg = 'error converting %s to Fo\n' % (xml_file)
            msg += 'command = "%s" \n' % (command)
            msg += output
            msg += '\n'
            error_func(msg)
    elif xsl_transform == 'xalan':
        command = 'xalan.sh '
        command += ' -OUT %s' % (out_file)
        command  += ' -IN %s -XSL %s' % (xml_file, xsl_file)
        params = param_dict.keys()
        for param in params:
            command += ' -PARAM %s "%s" ' % (param, param_dict[param])
        status, output = commands.getstatusoutput(command)
        if status:
            msg = 'error converting %s to Fo\n' % (xml_file)
            msg += 'command = "%s" \n' % (command)
            msg += output
            msg += '\n'
            error_func(msg)
    elif xsl_transform == 'lxml':
        error = transform_lxml(xslt_file = xsl_file, xml_file= xml_file, param_dict=param_dict, out_file= out_file)
        if error:
            error_func(error, xml_file)
    else:
        sys.stderr.write('"%s" not a valid xsl_transform. Fix. Script now quitting\n' % (xsl_transform))
        sys.exit(1)


def convert_to_xml(path_list):
    if verbose:
        sys.stderr.write('converting to xml...\n')
    num_files = len(path_list)
    counter = 0
    for the_path in path_list:
        counter += 1
        base, ext = os.path.splitext(the_path)
        out_file = '%s.xml' % (base)
        if verbose:
            sys.stderr.write('converting "%s" to "%s"\n' % (the_path, out_file))
        publish_xml_cmdline (in_path = the_path, out_path = out_file)

# simple, fail-proof method
def convert_to_xml_command(path_list):
    # print 'converting to xml...'
    num_files = len(path_list)
    counter = 0
    for the_path in path_list:
        counter += 1
        # print 'converting %s of %s files' % (counter, num_files)
        base, ext = os.path.splitext(the_path)
        out_file = '%s.xml' % (base)
        command = 'rst2xml.py --trim-footnote-reference-space %s %s' % (the_path, out_file)
        status, output = commands.getstatusoutput(command)

def convert_to_fo(xsl_transform):
    if verbose:
        sys.stderr.write('converting to fo...\n')
    xml_files =  glob.glob('*.xml')
    len_simple = len(xml_files)
    the_keys = test_dict.keys()
    for the_key in the_keys:
        if the_key not in xml_files:
            sys.stderr.write('%s  not found in test_files; fix. Script now quitting' % (the_key))
            sys.exit(1)
    len_complex = len(the_keys)
    len_inside = 0
    for the_key in the_keys:
        this_inside = len(test_dict[the_key])
        len_inside += this_inside
    num_files = len_simple - len_complex + len_inside
    counter = 0
    for xml_file in xml_files:
        params = {'strict':'True'}
        transform_info = test_dict.get(xml_file)
        if transform_info:
            for info in transform_info:
                counter += 1
                if verbose:
                    sys.stderr.write('converting "%s" to %s to FO\n' % (xml_file, out_file))
                # print 'converting %s of %s files' % (counter, num_files)
                added_params = info[0]
                out_file = info[1]
                params.update(added_params)
                transform_xsl(xsl_file = '../docutilsToFo/xsl_fo/docutils_to_fo.xsl', 
                        xml_file = xml_file, param_dict= params, out_file = out_file,
                        xsl_transform = xsl_transform)

                validate_the_fo(out_file)

        else:
            counter += 1
            # print 'converting %s of %s files' % (counter, num_files)
            base, ext = os.path.splitext(xml_file)
            out_file = '%s.fo' % (base)
            if verbose:
                sys.stderr.write('converting "%s" to %s FO\n' % (xml_file, out_file))
            transform_xsl(xsl_file = '../docutilsToFo/xsl_fo/docutils_to_fo.xsl', xml_file = xml_file, param_dict=params, xsl_transform = xsl_transform)
            validate_the_fo(out_file)

def convert_to_pdf():
    # print 'converting to pdf...'
    fo_files =  glob.glob('*.fo')
    num_files = len(fo_files)
    counter = 0
    for fo_file in fo_files:
        counter += 1
        # print 'converting %s of %s files' % (counter, num_files)
        base, ext = os.path.splitext(fo_file)
        out_file = '%s.pdf' % (base)
        command = 'fop  %s %s' % (fo_file, out_file)
        status, output = commands.getstatusoutput(command)
        if status:
            msg = 'error converting %s to PDF\n' % (fo_file)
            msg += 'command = "%s" \n' % (command)
            msg += output
            msg += '\n'
            error(msg)

def convert_fo_to_pdf(fo_file, out_file):
    command = 'fop  %s %s' % (fo_file, out_file)
    if verbose:
        sys.stderr.write('\n doing command "%s"\n' % (command))
    status, output = commands.getstatusoutput(command)
    if status:
        msg = 'error converting %s to PDF\n' % (fo_file)
        msg += 'command = "%s" \n' % (command)
        msg += output
        msg += '\n'
        error(msg)


# not used
def make_conf_file(the_dict, section = 'FO', conf_path = 'docutils.conf'):
    rm_path(conf_path)
    write_obj = file(conf_path, 'w')
    the_keys = the_dict.keys()
    write_obj.write('[%s]\n' % section) 
    for the_key in the_keys:
        write_obj.write('%s = %s\n' % (the_key, the_dict[the_key]))
    write_obj.close()
    return conf_path

def run_docutils_command(command):
    status, output = commands.getstatusoutput(command)
    if status:
        msg = 'error converting  to FO with docutilst_to_fo.py:\n' 
        msg += 'command = "%s" \n' % (command)
        msg += output
        msg += '\n'
        error_func(msg)

def validate_the_fo(xml_file):
    error = validate_fo_xsl(xml_file)
    if error:
        sys.stderr.write('Problems converting "%s"\n' % (xml_file))
        error_func('')

def test_docutils_to_fo_script(script_command):
    if verbose:
        sys.stderr.write('\n\n==========================================\n')
        sys.stderr.write('TESTING DOCUTILS SCRIPT\n')
        sys.stderr.write('\n==========================================\n')
    command = '%s -h' % script_command
    status, output = commands.getstatusoutput(command)
    default__commands = ' --config docutils.conf '
    for info in docfo_commands:
        command = script_command
        in_file = info[0]
        filename,ext = os.path.splitext(in_file)
        out_file = '%s.fo' % filename
        out_opt = ' -o %s ' % out_file
        config_file = os.path.join('conf_files', info[1])
        if not os.path.isfile(config_file):
            sys.stderr.write('Can\'t find "%s" conf file\n' % config_file)
            sys.exit(1)
        config_opt = ' --config %s ' % config_file
        command += ' %s %s %s' % (out_opt, config_opt, in_file)
        if verbose:
            sys.stderr.write('Converting "%s" to "%s" with "%s" \n' % (in_file, out_file, config_file))
        run_docutils_command(command)
        validate_the_fo(out_file)
        if PDF:
            out_pdf = '%s.pdf' % filename
            convert_fo_to_pdf(fo_file = out_file, out_file = out_pdf)


def clean():
    remove_paths(glob.glob('*.xml'))
    remove_paths(glob.glob('*.xsl'))
    remove_paths(glob.glob('*.fo'))
    remove_paths(glob.glob('*.pdf'))

def rm_path(the_path):
    try:
        os.remove(the_path)
    except OSError:
        pass

def remove_paths(list_of_paths):
    for the_path in list_of_paths:
        rm_path(the_path)

def full():
    # main()
    if verbose:
        sys.stderr.write('In FULL mode\n')
    clean()
    script_path  = os.path.abspath(os.path.join('scripts', 'docutils_to_fo.py'))
    script_command = 'python %s ' % script_path
    current_dir = os.getcwd()
    os.chdir('test_files')
    rst_files = glob.glob('*.rst')
    convert_to_xml(rst_files)
    xsl_trans = ['xsltproc', 'saxon',  'xalan', 'lxml',]
    for xsl_tran in xsl_trans:
        if verbose:
            sys.stderr.write('Transforming to FO with %s\n' % (xsl_tran))
        convert_to_fo(xsl_transform = xsl_tran)
        if xsl_tran != 'lxml':
            remove_paths(glob.glob('*.fo'))
    convert_to_pdf()
    test_docutils_to_fo_script(script_command)
    clean()
    os.chdir(current_dir)
    if verbose:
        sys.stderr.write('\n\nSUCCESS!\n')


def main():
    script_path  = os.path.abspath(os.path.join('scripts', 'docutils_to_fo.py'))
    script_command = 'python %s ' % script_path
    current_dir = os.getcwd()
    os.chdir('test_files')
    if not arg.no_rst:
        rst_files = glob.glob('*.rst')
        convert_to_xml(rst_files)
    if not arg.no_fo:
        convert_to_fo(xsl_transform = arg.xsl_transform[0])
    if PDF:
        convert_to_pdf()
    for fo_file in glob.glob('*.fo'):
        os.remove(fo_file)
    test_docutils_to_fo_script(script_command)
    #if arg.clean:
        # clean()
    # clean()
    os.chdir(current_dir)

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

if FULL:
    full()
else:
    main()

