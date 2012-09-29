import sys, os
from distutils.core import setup

scripts_dir = 'scripts'
if sys.version_info < (3,):
    sys.stderr.write('Sorry, but you must have Python version 3.0 or later\n')
    sys.exit(1)



setup(name='rst',
      version='.6',
      description='Adds functionality to Docutils XML',
      author='Paul Tremblay',
      # author_email='',
      # url='http://www.python.org/sigs/distutils-sig/',
      # packages=['distutils', 'distutils.command'],
      scripts = [os.path.join(scripts_dir, 'rstxml2xml.py')],
     )
# nothing
