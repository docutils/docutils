import os, re, sys, codecs

import xml.sax.saxutils
import xml.sax

# turn on this line if you want to disablenamespaces
##from xml.sax.handler import feature_namespaces

config_values = {}



class ConfigHandler(xml.sax.saxutils.DefaultHandler):
    """

    Class for handling the XML file. SAX uses the methods in this class to
    handle the data. 


    """
    def __init__(self):

        """
        

        Requires:


        Returns:

            nothing


        Logic:


        """
        self.__allowed = [ 'configuration', 'xslt-processor', ]


    
    def startElement(self, name, attrs):
        """

        Logic:

            The SAX driver uses this function when if finds a beginning tag.



        """
        if name in self.__allowed:
            values = attrs.values()
            config_values[name] = values
            
        else:
            sys.stderr.write('%s not a valid option\n' % name)

    
    def characters(self, character):
        """

        Logic:

            The SAX driver uses this function when it finds text.

        """
        pass

            
    
    def endElement(self, name):
        """

        Logic:


        """

        pass




class Configure:

    def __init__(self, file):


        """
        

        Requires:


        
        Returns:


        Logic:

            Set up a write object. 

            Create an instance of the InlineHandler for sax to use.

            Pass this instance to the SAX driver.

            Use the SAX driver to handle the file. 


        """
        file_exists = os.path.exists(file)
        if not file_exists:
            raise IOError, 'file %s does not exist' % file
        self.__file = file

    def read_configs(self):
        parser = xml.sax.make_parser()
        # turn on this line if you want to disable namespaces
        ##parser.setFeature(feature_namespaces, 0)
        config_handler = ConfigHandler()
        parser.setContentHandler(config_handler)
        parser.parse(self.__file)             
        return config_values


            
if __name__ == '__main__':
    file = '/home/paul/docutils-extensions/configure.xml'
    obj = Configure(file)
    obj.read_configs()
