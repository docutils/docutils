import paul.options, sys, os 
"""


"""


class XslConvert:

    def __init__(self, processor = 'xsltproc'):

        """



        """


        self.__write_to = '/home/paul/paultemp/xsl_convert_temp'

    def transform(self, file, xsl_file, output=None, indent_amount = 2):
        """
        Requires:

            file -- the file to parse

            xsl_file --the xsl style sheet

        Returns:

            nothing

        Logic:

            Make a tempory file to write to.

            Get the name of the file minus the extension, and make two new
            file names for later conversions.

        """
        command = 'xsltproc --param indent-amount %s %s %s > %s' % \
                (indent_amount, xsl_file, file, self.__write_to)
        os.system(command)
        if output:
            command = 'cat %s > %s' % (self.__write_to, output)
        else:
            command = 'cat %s' % self.__write_to

        os.system(command)
        os.remove(self.__write_to)

