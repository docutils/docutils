#! /usr/bin/env python

"""
:Author: David Goodger
:Contact: goodger@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This module has been placed in the public domain.

This package contains modules for language-dependent features of Docutils.
"""

__docformat__ = 'reStructuredText'

_languages = {}

def getlanguage(languagecode):
    if _languages.has_key(languagecode):
        return _languages[languagecode]
    module = __import__(languagecode, globals(), locals())
    _languages[languagecode] = module
    return module
