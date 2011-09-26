import sys, os, shutil, glob
from distutils.core import setup

def remove_build():
    if os.path.isdir('build'):
        shutil.rmtree('build')

# get the script name
def get_script_name():
    return os.path.join('scripts', 'docutils_to_fo.py')

xsl_files = glob.glob('xsl_fo/*')

def remove_files(the_list):
    for the_path in the_list:
        os.remove(the_path)

if 'sdist' in sys.argv:
    os.chdir('test_files')
    rm_files = []
    rm_files.extend(glob.glob('*.pdf'))
    rm_files.extend(glob.glob('*.fo'))
    rm_files.extend(glob.glob('*.xml'))
    rm_files.extend(glob.glob('*.xsl'))
    remove_files(rm_files)
    os.chdir('..')

remove_build()
script_name = get_script_name()

setup(name="docutilsToFo",
    version= '.6' ,
    description="Convert Docutils to FO",
    author="Paul Tremblay",
    author_email="noone@nowhere.com",
    license = 'GNU GPL',
    # url = "http://rtf2xml.sourceforge.net/",
    packages=['docutilsToFo'],
    package_dir = {'docutilsToFo': 'docutilsToFo'},
    package_data={'docutilsToFo': ['xsl_fo/*.xsl', 'valid/*.dtd', 'valid/*.rng', 'valid/*xsl']},
    scripts=[script_name],
    )
