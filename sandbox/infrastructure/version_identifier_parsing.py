#!/usr/bin/env python
# version_identifier_parsing.py: parse and sort Docutils version indentifiers
# ===========================================================================

# Parse a Docutils version identifier
# (adapted from the `PEP 440 example regexp`__)
#
# __https://www.python.org/dev/peps/pep-0440/#appendix-b-parsing-version-strings-with-regular-expressions


import re, argparse

from docutils import VersionInfo, __version__, __version_info__


_version_regexp = re.compile(r"""
                             (?P<release>
                               (?P<major>[0-9]+)
                               \.(?P<minor>[0-9]+)
                               (\.(?P<micro>[0-9]+))?
                             )
                             (?P<pre>               # pre-release segment
                               (?P<pre_l>(a|b|rc))
                               (?P<pre_n>[0-9]+)?
                             )?
                             (\.(?P<dev>dev))?      # dev segment
                             $
                             """, re.VERBOSE)

def parse_version_identifier(identifier):
    """Parse a Docutils version identifier according to PEP 440."""
    return _version_regexp.match(identifier).groupdict()


releaselevels = {'a':  'alpha',
                 'b':  'beta',
                 'rc': 'candidate',
                 '':   'final',
                }


def identifier2version_info(identifier):
    """Convert Docutils version identifier to a `version_info` namedtuple.
    """
    try:
        segments = _version_regexp.match(identifier).groupdict()
    except AttributeError:
        raise ValueError('non-supported version identifier "%s"' % identifier)

    if segments['pre']:
        releaselevel = releaselevels[segments['pre_l']]
    else:
        # .dev without pre-release segment sorts before pre-releases, see
        # https://www.python.org/dev/peps/pep-0440/#summary-of-permitted-suffixes-and-relative-ordering
        if segments['dev']:
            releaselevel = None
        else:
            releaselevel = 'final'

    return VersionInfo(
            major=int(segments['major']),
            minor=int(segments['minor']),
            micro=int(segments['micro']) if segments['micro'] else 0,
            releaselevel=releaselevel,
            serial=segments['pre_n'] and int(segments['pre_n']) or 0,
            release=not segments['dev'])


def version_info2identifier(version_info):
    """Return version identifier matching the given `docutils.version_info`."""
    release_level_abbreviations = dict((level, abbr)
                                for abbr, level in releaselevels.items())
    identifier = '%s.%s%s' % (version_info.major, version_info.minor,
                '.%s' % version_info.micro if version_info.micro else '')
    try:
        identifier += release_level_abbreviations[version_info.releaselevel]
    except KeyError:
        if version_info.releaselevel is not None or version_info.release:
            raise ValueError('releaselevel "%s" not supported. '
                             'Must be one of %s'
                             % (version_info.releaselevel,
                                ', '.join(releaselevels.values())))
    if version_info.releaselevel and version_info.serial:
        identifier += str(version_info.serial)
    if not version_info.release:
        identifier += '.dev'
    return identifier

version_info_def_template = """\
__version_info__ = VersionInfo(
    major=%d,
    minor=%d,
    micro=%d,
    releaselevel='%s', # one of 'alpha', 'beta', 'candidate', 'final'
    # pre-release serial number (0 for final releases and active development):
    serial=%d,
    release=%s # True for official releases and pre-releases
    )
"""

version_info_def_pattern = version_info_def_template.replace('%d', '[0-9]+')
version_info_def_pattern = version_info_def_pattern.replace('%s', '.*')
version_info_def_pattern = version_info_def_pattern.replace('(', r'\(')
version_info_def_pattern = version_info_def_pattern.replace(')', r'\)')

def version_info_definition(version_identifier):
    """Return __version_info__ definition code matching `version_identifier`.
    """
    versioninfo = identifier2version_info(version_identifier)
    return version_info_def_template % versioninfo


def change_version_info_definition(version, source):
    """Replace the __version_info__ definition in file "source" with
    a version matching the version identifer `version`."""
    version_info_def = version_info_definition(version)
    sourcefile = open(source)
    old = sourcefile.read()
    sourcefile.close()
    new = re.sub(version_info_def_pattern, version_info_def, old)
    if old == new:
        return "nothing to change (or parsing error)"
    sourcefile = open(source, 'w')
    sourcefile.write(new)
    sourcefile.close()
    return "changed %s" % source


# -----------------------------------------------------------------------

def test_parse(identifier):

    segments = parse_version_identifier(identifier)
    print identifier
    print 'release:', segments['release']
    print '    major:', segments['major']
    print '    minor:', segments['minor']
    print '    micro:', segments['micro']
    print '  pre:', segments['pre']
    print '    pre_l:', segments['pre_l']
    print '    pre_n:', segments['pre_n']
    print '  dev:', segments['dev']
    print

def selftest(version=__version__):
    """Run a test on version identification parsing and transforming."""

    # example: series of release identifiers in version-order.
    identifiers = ['0.13.1',
                '0.14.dev',
                '0.14a.dev',
                '0.14b.dev',
                '0.14b',
                '0.14rc1',
                '0.14rc2.dev',
                '0.14rc2',
                '0.14',
                '0.14.1rc1.dev',
                '0.14.1rc1',
                '0.14.1.dev',
                '0.14.1',
                ]

    for identifier in identifiers:
        test_parse(identifier)

    version_infos = [identifier2version_info(identifier)
                    for identifier in identifiers]

    for vi in version_infos:
        print vi
    print

    # test sort order:

    sorted_version_infos = sorted(version_infos)

    if sorted_version_infos == version_infos:
        print "Version order preserved by sorting."
    else:
        print "Version order changed by sorting."
        for vi in sorted_version_infos:
            print vi
    print

    # (re)convert version_info to PEP 440 version identifier:

    ids = [version_info2identifier(vi) for vi in version_infos]

    if ids == identifiers:
        print "Round trip conversion OK."
    else:
        print ids


    print version_info_definition(version)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('-i', '--version-info',
                        help='print version-info definition',
                        action='store_true')
    parser.add_argument('--change-version-info',
                        dest='source', metavar='SOURCE',
                        help='change version-info def in file "source"')
    parser.add_argument('-v', '--version',
                        help='version identifier '
                        '(default docutils.__version__)')
    parser.add_argument('-t', '--test',
                        help='test version indentification conversion',
                        action='store_true')
    parser.add_argument('--debug',
                        help='print result of version identifier parsing',
                        action='store_true')
    args = parser.parse_args()

    version_identifier = args.version or __version__
    if args.test:
        selftest(version_identifier)
    elif args.debug:
        from pprint import pprint
        pprint(_version_regexp.match(version_identifier).groupdict())
    elif args.version_info:
        print version_info_definition(version_identifier)
    elif args.source:
        print change_version_info_definition(version_identifier, args.source)
