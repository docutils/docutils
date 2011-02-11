import sys, os, shutil, glob
from distutils.core import setup

def remove_build():
    if os.path.isdir('build'):
        shutil.rmtree('build')

# get the script name
def get_script_name():
    return os.path.join('scripts', 'docutils_to_fo.py')

xsl_files = glob.glob('xsl_fo/*')

remove_build()
script_name = get_script_name()

setup(name="docutilsToFo",
    version= '.6' ,
    description="Convert Docutils to FO",
    author="Paul Tremblay",
    # author_email="phthenry@iglou.com",
    license = 'GNU GPL',
    # url = "http://rtf2xml.sourceforge.net/",
    packages=['docutilsToFo'],
    package_dir = {'docutilsToFo': 'docutilsToFo'},
    package_data={'docutilsToFo': ['xsl_fo/*.xsl', 'valid/docutils.dtd']},
    scripts=[script_name],
    )
