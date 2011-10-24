import sys
if sys.version_info < (3,):
    sys.stderr.write('Only run with pyton 3\n')
    sys.stderr.write('Script now quiting\n')
    sys.exit(1)

def make_the_unicode(start_num, end_num):
    for i in range(1,10):
        char_num = start_num + i
        sys.stdout.write(chr(char_num))

make_the_unicode(100, 200)
