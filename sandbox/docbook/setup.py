import sys, os
from distutils.core import setup

scripts_dir = 'scripts'
if sys.version_info >= (3,):
    from distutils.util import copydir_run_2to3
    copydir_run_2to3('scripts', 'scripts_ver3')
    scripts_dir = 'scripts_ver3'



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
