import sys, os
from subprocess import Popen, PIPE


class NotValidFoExeption(Exception):
    pass

class Fop():

    def __init__(self):
        fop_home = os.environ.get('FOP_HOME')
        if not fop_home:
            raise OSError('You must set the "FOP_HOME" variable')
        # plat = os.uname()
        # plat = platform.system()
        plat = os.name
        if  plat == 'posix':
            fop_bin = os.path.join(fop_home, 'fop')
        else:
            fop_bin = os.path.join(fop_home, 'fop.bat')
        self.fop_bin = fop_bin

    def to_pdf(self, fo_file, out_file = None):
        if not out_file:
            filename, ext = os.path.splitext(fo_file)
            out_file = '{0}.pdf'.format(filename)
        command_list = [self.fop_bin, '-fo', fo_file, '-pdf', out_file]
        p = Popen(command_list, stdout=PIPE, stderr=PIPE)
        stdout, stderr = p.communicate()
        if p.returncode:
            print(stderr)
        # on windows check for existence of pdf file

if __name__ == '__main__':
    fop_obj = Fop()
    fop_obj.to_pdf(sys.argv[1])
