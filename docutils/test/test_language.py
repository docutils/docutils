#!/usr/bin/python

# Author:    engelbert gruber
# Contact:   grubert@users.sourceforge.net
# Revision:  $Revision$
# Date:      $Date$
# Copyright: This module has been placed in the public domain.


"""
Test module for language modules for completeness.
"""

# i am unsure about how to test:
# the result i want is a test per lanuage.
# because this way the first error stops it.

import unittest
import docutils.languages
from docutils.parsers.rst import directives
import docutils.parsers.rst.languages

debug = 0

import os, re

class LanguageTests(unittest.TestCase):

    def _get_installed_languages(self):
        """
        get installed language translations from docutils.language.
        One might also get from parsers.rst.language.
        """
        lang_re = re.compile('^([a-z]{2}(_[A-Z]{2})?)\.py$')
        self.languages_found = []
        for mod in os.listdir(globals()['docutils'].languages.__path__[0]):
            match = lang_re.match(mod)
            if match and not match.group(1) == self.reference_language:
                self.languages_found.append(match.group(1))
        print self.languages_found

    def setUp(self):
        self.reference_language = 'en'
        self._get_installed_languages()
        self.ref = docutils.languages.get_language(self.reference_language)

    def debug(self):
        print "Reference language:", self.reference_language

    def _xor(self,ref_dict,l_dict):
        """
        Returns entries that are only in one dictionary.
        (missing_in_lang, more_than_in_ref).
        """
        missing  = []   # in ref but not in l.
        too_much = []   # in l but not in ref.
        for label in ref_dict.keys():
            if not l_dict.has_key(label):
                missing.append(label)
        for label in l_dict.keys():
            if not ref_dict.has_key(label):
                too_much.append(label)
        return (missing,too_much)

    def _test_label_translations(self,lang):
        """check label translations."""
        l = docutils.languages.get_language(lang)

        (missed,too_much) = self._xor(self.ref.labels,l.labels)
        if len(missed)>0 or len(too_much)>0:
            raise NameError, "Missed: %s, Too much: %s" %(str(missed),str(too_much))

#    def test_bibliographic_fields(self):
#       print "  bibliographic_fields length compare"
#       if not len(ref.bibliographic_fields) == len(l.bibliographic_fields):
#           print "  mismatch: ref has %d entries language %d." % \
#               (len(ref.bibliographic_fields), len(l.bibliographic_fields))

    def _test_directives(self,lang):
        l_directives = docutils.parsers.rst.languages.get_language(lang)
        failures = ""
        for d in l_directives.directives.keys():
            try:
                func, msg = directives.directive(d, l_directives, None)
            except:
                failures += "%s," % d 
        if not failures == "":
            raise NameError, failures

    def test_language(self):
        for lang in self.languages_found:
            self._test_label_translations(lang)
            self._test_directives(lang)
            

if __name__ == '__main__':
    import sys
    # and get language to test
    unittest.main()
    



# vim: set et ts=4 ai :
        
