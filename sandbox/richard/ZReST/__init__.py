# 
# $Id$
#
__version__='1.0'

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
# Revision 1.2  2002/08/15 04:36:56  richard
# FTP interface and Reporter message snaffling
#
# Revision 1.1  2002/08/14 05:15:37  richard
# Zope ReStructuredText Product
#
#
#
# vim: set filetype=python ts=4 sw=4 et si
