# -*- coding: utf-8 -*-
"""
    Sphinx - Python documentation webserver
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: 2007 by Armin Ronacher.
    :license: Python license.
"""
import os
import sys
import signal
import getopt

import sphinx
from sphinx.web.application import setup_app
from sphinx.web.serve import run_simple

try:
    from werkzeug.debug import DebuggedApplication
except ImportError:
    DebuggedApplication = lambda x, y: x

def check_superuser(orig_app):
    """Check if there is a superuser and create one if necessary."""
    if not orig_app.userdb.users:
        print 'Warning: you have no user database or no master "admin" account.'
        create = raw_input('Do you want to create an admin account now? [y/n] ')
        if not create or create.lower().startswith('y'):
            import getpass
            print 'Creating "admin" user.'
            pw1 = getpass.getpass('Enter password: ')
            pw2 = getpass.getpass('Enter password again: ')
            if pw1 != pw2:
                print 'Error: Passwords don\'t match.'
                sys.exit(1)
            orig_app.userdb.set_password('admin', pw1)
            orig_app.userdb.privileges['admin'].add('master')
            orig_app.userdb.save()

def main(argv):
    opts, args = getopt.getopt(argv[1:], "dh")
    opts = dict(opts)
    if not args or '-h' in opts:
        print 'usage: %s [-d] <doc_root> [<hostname> [<port>]]' % argv[0]
        print ' -d: use werkzeug debugger if installed'
        return 2

    port = 3000
    hostname = 'localhost'
    if len(args) > 1:
        hostname = args[1]
        if len(args) > 2:
            port = int(args[2])
    debug = ('-d' in opts) or (hostname == 'localhost')

    def make_app():
        orig_app, app = setup_app({
            'data_root_path':   args[0],
            'debug':            debug
        })
        check_superuser(orig_app)

        if debug:
            app = DebuggedApplication(app, True)
        return app

    if os.environ.get('RUN_MAIN') != 'true':
        print '* Sphinx %s- Python documentation web application' % \
              sphinx.__version__.replace('$', '').replace('Revision:', 'rev.')
        if debug:
            print '* Running in debug mode'

    run_simple(hostname, port, make_app, use_reloader=debug)


if __name__ == '__main__':
    sys.exit(main(sys.argv))
