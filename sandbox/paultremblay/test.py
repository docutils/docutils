import sys, os, glob, commands, shlex, subprocess
import docutilsToFo.rst2xml_lib

# $Id$ 
XSL_TRANSFORM = 'saxon'
XSL_TRANSFORM = 'xsltproc'
STRICT = True
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
                ({'page-layout': 'first', 'suppress-first-page-header': 'True', 'suppress-first-page-header': 'True'}, 'first_suppress_header__footer.fo'),
                ({'page-layout': 'odd-even' }, 'odd_even_page_header_footer.fo'),
                ({'page-layout': 'first-odd-even' }, 'first-odd_even_page_header_footer.fo'),
                ({'page-layout': 'first-odd-even', 'suppress-first-page-header': 'True', 'suppress-first-page-header': 'True'}, 'first_odd_even_page_header_footer_suppress_first.fo'),
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
            ]
        }

def error(msg):
    sys.stderr.write(msg)
    if STRICT:
        # sys.exit(1)
        pass

def transform_xsl(xsl_file, xml_file, param_dict = {}, out_file = None):
    if not out_file:
        base, ext = os.path.splitext(xml_file)
        out_file = '%s.fo' % (base)
    if XSL_TRANSFORM == 'xsltproc':
        command = 'xsltproc -o %s ' % (out_file)
        params = param_dict.keys()
        for param in params:
            command += ' --stringparam %s "%s" ' % (param, param_dict[param])
        command += ' %s %s ' % (xsl_file, xml_file)
        status, output = commands.getstatusoutput(command)
        if status:
            msg = 'error converting %s to FO\n' % (xml_file)
            msg += 'command = "%s" \n' % (command)
            msg += output
            msg += '\n'
            error(msg)
    elif XSL_TRANSFORM == 'saxon':
        command = 'saxon.sh '
        command += ' -o %s' % (out_file)
        if STRICT:
            command += ' -warnings:fatal '
        command  += ' %s %s' % (xml_file, xsl_file)
        params = param_dict.keys()
        for param in params:
            command += ' %s=%s ' % (param, param_dict[param])
        status, output = commands.getstatusoutput(command)
        if status:
            msg = 'error converting %s to Of\n'
            msg += 'command = "%s" \n' % (command)
            msg += output
            msg += '\n'
            error(msg)

def main():
    current_dir = os.getcwd()
    os.chdir('test_files')
    rst_files = glob.glob('*.rst')
    convert_to_xml(rst_files)
    convert_to_fo()
    convert_to_pdf()
    os.chdir(current_dir)

def convert_to_xml(path_list):
    print 'converting to xml...'
    num_files = len(path_list)
    counter = 0
    xml_obj = docutilsToFo.rst2xml_lib.XMLPublish()
    for the_path in path_list:
        counter += 1
        print 'converting %s of %s files' % (counter, num_files)
        base, ext = os.path.splitext(the_path)
        out_file = '%s.xml' % (base)
        xml_obj.publish_xml_cmdline(writer_name='xml', 
                source=the_path,
                destination = out_file
                )

def convert_to_xml_old(path_list):
    for the_path in path_list:
        base, ext = os.path.splitext(the_path)
        out_file = '%s.xml' % (base)
        command = 'rst2xml.py --trim-footnote-reference-space %s %s' % (the_path, out_file)
        status, output = commands.getstatusoutput(command)

def convert_to_fo():
    print 'converting to fo...'
    params = {'strict':'True'}
    xml_files =  glob.glob('*.xml')
    num_files = 60
    counter = 0
    for xml_file in xml_files:
        counter += 1
        print 'converting %s of %s files' % (counter, num_files)
        transform_info = test_dict.get(xml_file)
        if transform_info:
            for info in transform_info:
                counter += 1
                added_params = info[0]
                out_file = info[1]
                params.update(added_params)
                transform_xsl('../xsl_fo/docutils_to_fo.xsl', xml_file, params, out_file)
        else:
            transform_xsl('../xsl_fo/docutils_to_fo.xsl', xml_file, params)

def convert_to_pdf():
    print 'converting to pdf...'
    fo_files =  glob.glob('*.fo')
    num_files = len(fo_files)
    counter = 0
    for fo_file in fo_files:
        counter += 1
        print 'converting %s of %s files' % (counter, num_files)
        base, ext = os.path.splitext(fo_file)
        out_file = '%s.pdf' % (base)
        command = 'fop  %s %s' % (fo_file, out_file)
        status, output = commands.getstatusoutput(command)


try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass
main()

