# WARNING TO PAUL: DON'T EDIT THIS FILE use update.py instead.
import sys, os, tempfile




"""


"""


class XslConvert:

    def __init__(self, processor = 'xsltproc'):

        """



        """
        self.__determine_processor(processor)



    def __determine_processor(self, processor):
        if processor == 'xsltproc' or  processor == 'xmllint':
            self.__processor = 'xmllint'
        elif processor == '4suite':
            self.__processor = '4suite'
        elif processor == 'xalan':
            self.__processor = 'xalan'
        else:
            sys.stderr.write('%s not a valid processor choice\n' % processor)
            sys.exit(1)
    
    def transform(self, file, xsl_file, output, params = {}):
        """
        Requires:

            file -- the file to parse

            xsl_file --the xsl style sheet

        Returns:

            nothing

        Logic:

            Check for the existence of the files and the stylesheet. Determine
        the processor to be used, and let the othe methods do the processing.

        """

        if not (os.path.exists(file)):
            sys.stderr.write('"%s" does not exist\n' % file)
            sys.exit(1)
        if not (os.path.exists(xsl_file)):
            sys.stderr.write('"%s" does not exist\n' % xsl_file)
            sys.exit(1)

        if self.__processor == 'xmllint':
            self.__transform_xmllint(file, xsl_file, output, params)
        if self.__processor == 'xalan':
            self.__transform_xalan(file, xsl_file, output, params)

        elif self.__processor == '4suite':
            self.__transform_4suite(file, xsl_file, output, params)

    def __transform_xmllint(self, file, xsl_file, output, params = {}):
        import libxml2
        import libxslt

        new_params = {}
        keys = params.keys()
        for key in keys:
            new_params[key] = '"%s"' % params[key]
        params = new_params
            


        xml_doc = file
        # parse stylesheet
        styledoc = libxml2.parseFile(xsl_file)
        style = libxslt.parseStylesheetDoc(styledoc)
        # parse doc
        doc = libxml2.parseFile(xml_doc)
        result = style.applyStylesheet(doc, params)
        style.saveResultToFilename(output, result, 0)
        style.freeStylesheet()
        doc.freeDoc()
        result.freeDoc()



    def __transform_4suite(self, file, xsl_file, output, params):

        import codecs
        from Ft.Xml import InputSource
        from Ft.Xml.Xslt.Processor import Processor

        document = InputSource.DefaultFactory.fromUri(file)
        stylesheet = InputSource.DefaultFactory.fromUri(xsl_file)
            # there's also a fromString() method

        processor = Processor()
        processor.appendStylesheet(stylesheet)  
        result = processor.run(document, topLevelParams=params)
        (utf8_encode, utf8_decode, utf8_reader, utf8_writer) = codecs.lookup("utf-8")
        write_obj = utf8_writer(open(output, 'w'))
        write_obj.write(result)
        write_obj.close()

    def __transform_xalan(self, file, xsl_file, output, params):
        command = 'java org.apache.xalan.xslt.Process \
        -Ts -in %s -xsl %s -out %s' %  (file, xsl_file, output)
        
        param_string = ''
        keys = params.keys()
        for key in keys:
            param_string += ' %s "%s"' % (key, params[key])
            
        command += ' -PARAM %s' % param_string
        print command
        os.system(command)


        
if __name__ == '__main__':
    test_xml_dir = '/home/paul/lib/python/xml_tools_trem/test_files/xml_files'
    test_xsl_dir = '/home/paul/lib/python/xml_tools_trem/test_files/xsl_stylesheets'
    output_dir = '/home/paul/paultemp'

    file = 'simple.xml'
    xsl_file = 'simple1.xsl'
    output = 'output.xml'


    test_file = os.path.join(test_xml_dir, file)
    test_xsl = os.path.join(test_xsl_dir, xsl_file)
    test_output = os.path.join(output_dir, output)

    test_obj = XslConvert('xalan')
    test_obj.transform (test_file, test_xsl, test_output, params = {'test-param': 'changed in xalan'})
