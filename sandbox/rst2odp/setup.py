from distutils.core import setup
#from setuptools import setup

from types import ModuleType
script = ModuleType("rst2odp")
exec open("bin/rst2odp") in script.__dict__

setup(name="rst2odp",
      version=script.__version__,
      author=script.__author__,
      description="Converter for rst to OpenOffice Impress",
      scripts=["bin/rst2odp"],
      package_dir={"odplib":"odplib"},
      package_data={'odplib':['data/*.xml']},
      packages=['odplib'],
)
           
