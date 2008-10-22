from distutils.core import setup
#from setuptools import setup

from types import ModuleType
script = ModuleType("rst2odp")
exec open("bin/rst2odp") in script.__dict__

setup(name="rst2odp",
      version=script.__version__,
      author=script.__author__,
      description="Create odp slides from rst",
      scripts=["bin/rst2odp"],
      package_dir={"rst2odp":"rst2odp"},
      packages=['rst2odp'],
)
           
