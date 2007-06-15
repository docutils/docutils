#!/usr/bin/env python
# -*- coding: iso-8859-1 -*-

# :Copyright: 2006 Guenter Milde.
#             Released without any warranty under the terms of the 
#             GNU General Public License (v. 2 or later)

# prepend parent dir to the PYTHONPATH
import sys, os.path
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from latex2e_adaptive_preamble import *

class TestPackages(object):
    """test the LaTeX Package administrator class"""
    
    def setUp(self):
        self.pkgs = Packages()
    
    def test_optionstring(self):
        result = self.pkgs._optionstring(['soffi', 'moffi'])
        print result
        assert result == '[soffi,moffi]'
        
    def test_optionstring_empty(self):
        """return empty string for empty optionlist"""
        assert self.pkgs._optionstring([]) == ''
        
    def test_optionstring_empty_content(self):
        assert self.pkgs._optionstring(['']) == ''
        assert self.pkgs._optionstring(['', None]) == ''

    def test_require(self):
        print "empty packages dict", self.pkgs.register
        assert self.pkgs.register == {}, "packages dict should be empty"
        self.pkgs.require("muckla")
        print "with 'muckla'", self.pkgs.register
        assert self.pkgs.register['muckla'] == ()
        self.pkgs.require("hens", 'soffi', 'moffi')
        print "with 'hens'", self.pkgs.register
        assert self.pkgs.register['hens'] == ('soffi', 'moffi')
        
    def test_require_twice(self):
        """calling Packages.require() twice with the same package 
        should result in only one package entry"""
        self.pkgs.require("mula")
        self.pkgs.require("mula")
        print "with 'mula'", self.pkgs.register
        assert self.pkgs.register['mula'] == ()
        assert len(self.pkgs.register) == 1, "one package registered"

    def test_call(self):
        # return empty list if no packages are required
        assert self.pkgs() == []
        # return list of packages
        self.pkgs.require("mole")
        self.pkgs.require("hens", 'soffi', 'moffi')
        lines = self.pkgs()
        print lines
        # order of lines is undefined
        assert len(lines) == 2, "should return 2 lines"
        assert '\\usepackage{mole}\n' in lines, "should use 'mole'"
        assert '\usepackage[soffi,moffi]{hens}\n' in lines, "should use 'hens'"
        del self.pkgs

    def test_str(self):
        """should return latex commands for inclusion of required packages"""
        assert str(self.pkgs) == ''
        self.pkgs.require("mouse")
        self.pkgs.require("hens", 'soffi', 'moffi')
        result = str(self.pkgs)
        print result
        # order of lines is undefined
        lines = result.splitlines(True)
        assert len(lines) == 2, "should return 2 lines"
        assert '\\usepackage{mouse}\n' in lines, "should use 'mouse'"
        assert '\usepackage[soffi,moffi]{hens}\n' in lines, "should use 'hens'"
        del self.pkgs
        

if __name__ == "__main__":
    import nose
    nose.runmodule() # requires nose 0.9.1
    sys.exit()
