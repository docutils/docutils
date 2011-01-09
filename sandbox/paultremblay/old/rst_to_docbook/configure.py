# /usr/bin/env python 

import sys, os
import rst_to_docbook.options_trem

"""

The configuration script gets the target from the command line. It creates a
file with the configuration variable, and a short script for the rest of
script to be able to read and locate he configuration files.

"""

def configure():
    target, processor = get_target()
    if processor == None:
        sys.stderr.write('Since you need an xslt processor to run this script, the configuration will now quit\n'
                )
        try:
            os.remove('var_file')
        except:
            pass
        sys.exit(1)
    make_var_file(target)
    make_location(target) 
    make_configuration_file(processor)

def get_target():
    """
    This functions uses a module I wrote to parse options. If no options are
    determined on the command line, the function returnst the default
    /etc/nest_docutis

    """
    options_dict = {
        'target':     [1, 't'],
        'processor':   [1, 'p'],
    }
    options_obj = rst_to_docbook.options_trem.ParseOptions(sys.argv, 
            options_dict)
    opt_dict, args = options_obj.parse_options()
    if opt_dict == 0:
        sys.stderr.write('invalid options for configure.py\n'
                'use python configure --target <desired folder>'
                ' --processor <xslt proccessor>'
                
                )
        sys.exit(1)
    target = opt_dict.get('target')
    if not target:
        target = default_target()
    processor = opt_dict.get('processor')
    processor = determine_processor(processor)
    return target, processor

def default_target():
    sys.stdout.write('using default \'/etc\' for the configuration directory\n')
    return '/etc'
    
def determine_processor(processor = None):
    sys.stdout.write('determining xslt processor...\n')
    if processor == None:
        processor = 'xmllint'
    if processor == 'xalan':
        file = 'test_files/simple.xml'
        xsl_file = 'test_files/simple.xsl'
        output = 'output.xml'
        command = 'java org.apache.xalan.xslt.Process \
    -Ts -in %s -xsl %s -out %s' %  (file, xsl_file, output)
        error = os.system(command)
        if error:
            sys.stderr.write('xalan does not appear to be set up correctly '
                    ' on your system\n'
                    'The command "java org.apache.xalan.xslt.Process" failed\n'
                    'Is the CLASSPATH set for xalan?\n'
                    'Configuraton will now quit\n'
                    )
            sys.exit(1)
        else:
            return 'xalan'
    elif processor == '4suite':
        try:
            from Ft.Xml import InputSource
            from Ft.Xml.Xslt.Processor import Processor
            return '4suite'
        except:
            sys.stderr.write('4suite does not appear to be set up correctly on your system\n'
                    'Could not find the Ft.Xml libraries\n'
                    'Script cannot work without an xslt procesor!\n'
                    )
            sys.exit(1)
    elif processor == 'xsltproc' or processor == 'xmllint':
        try:
            import libxml2
            import libxslt
            return 'xmllint'
        except:
            sys.stderr.write('You either choose xmllint as your processor, or xmllint was tested because not other\n'
                    'processor was found\n'
                    'However, the libraries "libxml2" and or "libxslt" cannot be found.\n'
                    )
            
    else:
        sys.stderr.write('The processor "%s" is not a valid choice for this script\n' % processor)
    return None


        
    
def make_var_file(target):
    write_obj = open('var_file', 'w')
    # write_obj.write('[global]\n')
    write_obj.write(target)
    write_obj.close()

def make_location(target):
    write_obj = open('rst_to_docbook/location.py', 'w')
    write_obj.write(
    """
def get_location():
    return '%s'


    """
    % target)

def make_configuration_file(processor):
    write_obj = open('data/configure.xml', 'w')
    write_obj.write("""
<configuration>
    <xslt-processor processor = "%s"/>
</configuration>
    """ % processor
            )

if __name__ == '__main__':
    configure()

