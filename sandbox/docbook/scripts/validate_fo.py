import os, sys
from subprocess import Popen, PIPE

class ValidateFo():

    def __init__(self):
        valid_home = os.environ.get('VALIDATE_HOME')
        if not valid_home:
            raise OSError('You must set the "VALIDATE_HOME" variable')
        xsl_stylesheet = os.path.join(valid_home, 'xslt', 'folint.xsl')
        if not os.path.isfile(xsl_stylesheet):
            raise OSError('Cannot find xsl_stylesheet')
        self.xsl_stylesheet = xsl_stylesheet

    def validate_fo(self, in_file):
        command_list = ['xsltproc', self.xsl_stylesheet, in_file]
        p = Popen(command_list, stdout=PIPE, stderr=PIPE)
        stdout, stderr = p.communicate()
        if len(stderr) != 0:
            sys.stderr.write(stderr)
            return False
        return True

if __name__ == '__main__':
    validate_obj = ValidateFo()
    validate_obj.validate_fo(sys.argv[1])
