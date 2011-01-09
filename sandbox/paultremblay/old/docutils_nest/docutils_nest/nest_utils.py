"""

This script converts a document marked up as rst to XML. It uses the
docutils-xml.py conversion tools, and then performs one more transformation to
convert text specially marked up to XML as well.

NOTE: This is not part of the official docutils package. Rather, it is my own
extension.

"""

import os, sys
import docutils_nest.nest_docutils
import docutils_nest.rst_options

class RstWithInline:

    def __init__(self, file, output, docutils_opts = []):
        self.__file = file
        self.__output = output
        self.__docutils_opts = docutils_opts

    def convert(self):

        """

        Requires:

            nothing

        Returns:

            nothing

        Logic:

            First convert the txt file to a temp XML file. Then convert this
            temp file, chagning inline marked-up text to tags.

        """
        temp_file = self.__convert_restructure()
        self.__convert_inline(temp_file)



    
    def __convert_restructure(self):
        """

        Requires:

            nothing

        Returns:

            nothing

        Logic:

            Use the docuitls-xml script to convert the file from rst to XML


        """
        import locale
        try:
            locale.setlocale(locale.LC_ALL, '')
        except:
            pass
        ##--no-doctype 
        argv = self.__docutils_opts
        argv.append('--no-doctype')
        argv.append(self.__file) # input file
        restructure_temp = '/tmp/restructure_text_temp' # temp output
        argv.append(restructure_temp) 


        from docutils.core import publish_cmdline, default_description

        # I think this is just for the description at the beginning of the
        # resulting file
        description = ('Generates Docutils-native XML from standalone '
                       'reStructuredText sources.  \n' 
                       'From script resruct_to_tei.py  ' + default_description)

        # invoke the method for coversion
        publish_cmdline(writer_name='xml', description=description,
                        argv = argv)

        return restructure_temp
        # os.rename(restructure_temp, self.__output)

    def __convert_inline(self, temp):

        """

        Requires:

            temp -- a valid, XML file converted from docutils-xml.py

        Returns:

            nothing

        Logic:

            Use the module nest_docutils to transform text marked up as inline to XML.

        """
        nest_obj = docutils_nest.nest_docutils.NestDocutils(temp, self.__output)
        nest_obj.nest_tags()




def print_help_message():
    sys.stdout.write(
            '\n\n'
            'Use For rst_convert_with_inline:\n'
            '================================='
            '\n\n'
            'python rst_convert_with_inline.py [docuitls options] --output <output> file.txt\n'
            '(--output *must* be provided!)\n'
            '\n\n'
            'An invalid docutils options will cause an error.\n'
            'Hold on while the docutils help message prints out...'
            '\n\n'
        )

    
    import locale
    try:
        locale.setlocale(locale.LC_ALL, '')
    except:
        pass
    argv = ['--help']

    from docutils.core import publish_cmdline, default_description

    # I think this is just for the description at the beginning of the
    # resulting file
    description = ('Generates Docutils-native XML from standalone '
                   'reStructuredText sources.  \n' 
                   'From script resruct_to_tei.py  ' + default_description)

    # invoke the method for coversion
    publish_cmdline(writer_name='xml', description=description,
                    argv = argv)
    
def convert_file():

    options_dict = {
        'output':       [1]
    }
    opt_obj = docutils_nest.rst_options.OptionsRst(sys.argv, options_dict)
    inline_opts, args, doc_opts = opt_obj.parse_options()
    if inline_opts == 0:
        print_help_message()
        sys.stderr.write('Script will now quite because of illegal options\n')
        sys.exit(1)
    try:
        file = args[0]
    except IndexError:
        sys.stderr.write('You must provide a file for the script to convert.\n'
                'Script will now quit'
                )
        sys.exit(1)
    output = inline_opts.get('output')
    if not output:
        sys.stderr.write('You must provide an output option for the script to work\n'
                'Script will now quit\n'
                )
        sys.exit(1)
    convert_obj = RstWithInline(file, output, docutils_opts = doc_opts)
    convert_obj.convert()
    
if __name__ == '__main__':
    # # file = os.path.join(nest_inline_location, 'nest_inline/test_files/simple1.txt')
    # # output = os.path.join(nest_inline_location, 'nest_inline/output.xml')
    convert_file()

    nest_inline_location = '/home/paul/lib/python/'
