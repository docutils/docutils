import sys, os, subprocess, glob

class ValidateDocbook():

    def __init__(self):
        jing_dir = os.environ.get('JING_DIR')
        if jing_dir == None:
            raise IOError('You need to set the variable "JING_DIR"')
        jing_jar = os.path.join(jing_dir, 'bin', 'jing.jar')
        if not os.path.isfile(jing_jar):
            raise IOError('You need download the jing.jar')
        self.jing_jar = jing_jar
        valid_home = os.environ.get('VALIDATE_HOME')
        if valid_home == None:
            raise IOError('You need to set the variable "VALIDATE_HOME"')
        docbook_rng = os.path.join(valid_home, 'relax', 'docbook.rng')
        if not os.path.isfile(docbook_rng):
            msg = 'cannot find "{0}"'.format(docbook_rng)
            msg += '\nYou need download docbook.rng'
            raise IOError(msg)
        self.docbook_rng = docbook_rng

    def is_valid(self, in_files):
        command_list = ['java', '-jar', self.jing_jar, self.docbook_rng]
        if isinstance(in_files, list):
            for f in in_files:
                command_list.append(f)
        else:
            command_list.append(in_files)
        exit_status = subprocess.call(command_list)
        if exit_status:
            return False
        return True

if __name__ == '__main__':
    in_files = sys.argv[1:]
    valid_obj = ValidateDocbook()
    valid = valid_obj.is_valid(in_files = in_files)
