# 
# $Id$
#
__version__='1.0'

import os
# figure where ZReST is installed
here = None
if os.environ.has_key('INSTANCE_HOME'):
    here = os.environ['INSTANCE_HOME']
    path = os.path.join(here, 'Products', 'ZReST')
    if not os.path.exists(path):
        path = os.path.join(here, 'lib', 'python', 'Products', 'ZReST')
        if not os.path.exists(path):
            here = None
if here is None:
    from __main__ import here
    path = os.path.join(here, 'Products', 'ZReST')
    if not os.path.exists(path):
        path = os.path.join(here, 'lib', 'python', 'Products', 'ZReST')
        if not os.path.exists(path):
            raise ValueError, "Can't determine where ZReST is installed"

# product initialisation
import ZReST
def initialize(context):
    context.registerClass(
        ZReST, meta_type = 'ReStructuredText Document',
        constructors = (
            ZReST.manage_addZReSTForm, ZReST.manage_addZReST
        )
    )


#
# $Log$
# Revision 1.1  2002/08/14 05:15:37  richard
# Zope ReStructuredText Product
#
#
#
# vim: set filetype=python ts=4 sw=4 et si
