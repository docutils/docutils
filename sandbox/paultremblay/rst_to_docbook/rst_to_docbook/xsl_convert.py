import paul.options, sys, os, tempfile




"""


"""


class XslConvert:

    def __init__(self, processor = 'xsltproc'):

        """



        """

        pass


    def transform(self, file, xsl_file, output, indent_amount = 2):
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
                (indent_amount, xsl_file, file, output)
        os.system(command)


