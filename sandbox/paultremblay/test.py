import sys, os, glob, commands, shlex, subprocess, argparse
import docutilsToFo.rst2xml_lib
from lxml import etree
import lxml
from docutilsToFo.rst2xml_lib import transform_lxml, publish_xml_cmdline

# $Id$ 
TEST = False
STRICT = True

parser = argparse.ArgumentParser(description='Test all the files') 
parser.add_argument('--xslt', nargs=1, choices = ['xsltproc', 'lxml', 'saxon'], default=['lxml'], 
        dest='xsl_transform', help = 'choose which processer to use when transforming' ) 
parser.add_argument('--no-pdf', action="store_const", const=True, help = 'don\'t convert to PDF', dest='no_pdf')
parser.add_argument('--no-rst', action="store_const", const=True, help = 'don\'t convert to RST', dest='no_rst')
parser.add_argument('--no-fo', action="store_const", const=True, help = 'don\'t convert to FO', dest='no_fo')
parser.add_argument('--strict', nargs=1, choices = ['True', 'False'],  
        default = 'True', help = 'whether to quit on errors', dest='strict')
parser.add_argument('--clean', action="store_const", const=True, help = 'whether to remove files after test', dest='clean')
arg = parser.parse_args()
if  arg.strict == 'True':
    STRICT = True
else:
    STRICT = False

# =========================================================================================
# stylesheet  test
# =========================================================================================
test_dict = {
        'long_plain.xml':[({'page-layout':'simple'}, 'simple_no_page_nos.fo'), 
                ({'page-layout': 'first'}, 'first_page_diff_no_page_nos.fo'),  
                ({'page-layout': 'odd-even'}, 'odd_even_no_page_nos.fo'),
                ({'page-layout': 'first-odd-even'}, 'first_odd_even_no_page_nos.fo')
                ],
        'simple_header_footer.xml':[({'page-layout': 'simple'}, 'header_footer.fo'),
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
        'table_csv.xml':[({},''),
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
            ]
        }
# =========================================================================================
# Docutils_to_fo test
# =========================================================================================

docfo_commands = [
            ('long_plain.xml', '' , {'paper-size.height':'9in', 'paper-size.width': '6in'}),
            ('long_plain.xml', '' , {'page.top-margin':'.75in', 'page.bottom-margine': '.75in', 
                'page.right-margin' : '.75in', 'page.left-margin' : '.75in' }),
            ('long_plain.xml', '', {'odd-page.top-margin' : '1in', 'odd-page.bottom-margin' : '1in', 
                'odd-page.right-margin' : '1in', 'odd-page.left-margin' : '2in', 'even-page.top-margin' : '1in', 
                'even-page.bottom-margin' : '1in', 'even-page.right-margin' : '2in', 
                'even-page.left-margin' : '1in', 'first-page.top-margin' : '3in', 
                'first-page.bottom-margin' : '1in', 'first-page.right-margin' : '.8in', 
                'first-page.left-margin' : '2in'}), 
                
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
    elif xsl_transform == 'lxml':
        error = transform_lxml(xslt_file = xsl_file, xml_file= xml_file, param_dict=param_dict, out_file= out_file)
        if error:
            error_func(error, xml_file)
    else:
        sys.stderr.write('"%s" not a valid xsl_transform. Fix. Script now quitting\n' % (xsl_transform))
        sys.exit(1)


def convert_to_xml(path_list):
    # print 'converting to xml...'
    num_files = len(path_list)
    counter = 0
    for the_path in path_list:
        counter += 1
        # print 'converting %s of %s files' % (counter, num_files)
        base, ext = os.path.splitext(the_path)
        out_file = '%s.xml' % (base)
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
    # print 'converting to fo...'
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
                # print 'converting %s of %s files' % (counter, num_files)
                added_params = info[0]
                out_file = info[1]
                params.update(added_params)
                transform_xsl(xsl_file = '../docutilsToFo/xsl_fo/docutils_to_fo.xsl', 
                        xml_file = xml_file, param_dict= params, out_file = out_file,
                        xsl_transform = xsl_transform)
        else:
            counter += 1
            # print 'converting %s of %s files' % (counter, num_files)
            transform_xsl(xsl_file = '../docutilsToFo/xsl_fo/docutils_to_fo.xsl', xml_file = xml_file, param_dict=params, xsl_transform = xsl_transform)

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
    status, output = commands.getstatusoutput(command)
    if status:
        msg = 'error converting %s to PDF\n' % (fo_file)
        msg += 'command = "%s" \n' % (command)
        msg += output
        msg += '\n'
        error(msg)


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

def test_docutils_to_fo_script(script_command):
    command = '%s -h' % script_command
    status, output = commands.getstatusoutput(command)
    default__commands = ' --config docutils.conf '
    for info in docfo_commands:
        command = script_command
        make_conf_file(info[2])
        in_file = info[0]
        filename,ext = os.path.splitext(in_file)
        out_file = '%s.fo' % filename
        out_opt = ' -o %s ' % out_file
        command += ' %s %s %s' % (info[1], out_opt, info[0])
        run_docutils_command(command)
        out_pdf = '%s.pdf' % filename
        convert_fo_to_pdf(fo_file = out_file, out_file = out_pdf)

def clean():
    xml_files = glob.glob('*.xml')
    for xml_file in xml_files:
        os.remove(xml_file)
    fo_files = glob.glob('*.fo')
    for fo_file in fo_files:
        os.remove(fo_file)
    pdf_files = glob.glob('*.pdf')
    for pdf_file in pdf_files:
        os.remove(pdf_file)

def rm_path(the_path):
    try:
        os.remove(the_path)
    except OSError:
        pass

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
    if not arg.no_pdf:
        convert_to_pdf()
    for fo_file in glob.glob('*.fo'):
        os.remove(fo_file)
    test_docutils_to_fo_script(script_command)
    if arg.clean:
        clean()
    os.chdir(current_dir)

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

main()

