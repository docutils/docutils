#!/usr/bin/env python3
# :Copyright: © 2022 Günter Milde.
# :License: Released under the terms of the `2-Clause BSD license`_, in short:
#
#    Copying and distribution of this file, with or without modification,
#    are permitted in any medium without royalty provided the copyright
#    notice and this notice are preserved.
#    This file is offered as-is, without any warranty.
#
# .. _2-Clause BSD license: https://opensource.org/licenses/BSD-2-Clause

# :Id: $Id:  $

"""A new ConfigParser version for docutils/frontend.py

Clear separation of duties for OptionParser and ConfigParser.

The ConfigParser:

* Reads and parses the sequence of configuration files passed to its
  `read()` method.

* Stores the extracted setting values as stings.

* Collects settings from "active sections" in its ACTIVE section.

The OptionParser:

* Validates the settings and converts the values to the expected data type.

* Eventually updates the settings from command line arguments.
"""

# TODO: Validate configuration file settings in the ConfigParser?
# 
# +1 Collecting "appending_settings" becomes easier.
# 
# -1 Require inheriting configparser.RawConfigParser to allow storing
#    values other than strings. 
#    (Alternative: collect in ``dict`` instead of ACTIVE section.)
# 
# -1 Requires knowledge of all validation functions and converters
#    in the ConfigParser.

import collections
import configparser

class ConfigParser(configparser.ConfigParser):
    """Parser for Docutils configuration files.

    See https://docutils.sourceforge.io/docs/user/config.html.

    Option key normalization includes conversion of '-' to '_'.

    Config file encoding is "utf-8". Encoding errors are reported
    and the affected file(s) skipped.

    There is an additional section ACTIVE that contains settings
    from all "active sections".
    """

    not_utf8_error = ('Unable to read configuration file "%s": '
                      'content not encoded as UTF-8.\n'
                      'Skipping "%s" configuration file.')

    def __init__(self, *args,
                 active_sections=(),
                 encoding_settings = (),
                 appending_settings = None, # {<setting>: <separator>}
                 overriding_settings = None, # {<overriding>: <overridden>}
                 interpolation=None,   # no interpolation by default
                 **kwargs):
        """Initialize configuration file parser.

        `active_sections` lists sections whose values are stored in ACTIVE.

        `appending_settings` maps settings that do not override the current
        value in the ACTIVE section to the list separator (',' or ':') that
        is used to append them instead.

        `encoding_settings` lists settings that set the corresponding
        encoding error handler setting if their value contains a colon (':')
        or re-set it if there is no colon.

        `overriding_settings` maps settings that reset other settings
        from the same configuration file to the overridden setting.

        TODO: add a section for settings specifications?
              The settings specifications would then be parsed to
              get values for the above settings.
        """
        self.active_sections = tuple(# remove duplicates, keep order
                                     dict.fromkeys(active_sections))
        self.appending_settings = appending_settings or {}
        self.encoding_settings = encoding_settings
        self.overriding_settings = overriding_settings or {}
        super().__init__(*args, interpolation=interpolation, **kwargs)

        # Section with union of active section's settings
        # TODO: use a name that cannot be set in a config file?
        self['ACTIVE'] = {}
        # internal dictionary of appending settings:
        self._appending_setting_lists = collections.defaultdict(list)

    def clear(self):
        """Clear all settings. Restore ACTIVE section.

        The DEFAULT and the ACTIVE sections are always present.
        Exception: after an explicit `<instance>['ACTIVE'].clear()`.
        """
        self._appending_setting_lists.clear()
        super().clear()
        self['ACTIVE'] = {}

    def read(self, filenames):
        read_ok = []
        if isinstance(filenames, str):
            filenames = [filenames]
        for filename in filenames:
            # Config files are UTF-8-encoded:
            try:
                read_ok += super().read(filename, encoding='utf-8')
            except UnicodeDecodeError:
                sys.stderr.write(self.not_utf8_error % (filename, filename))
                continue
            self.handle_side_effects()
            self.collect_active_settings()
        return read_ok

    def handle_side_effects(self):
        # handle settings with side-effects
        for section in self.values():
            for key, value in dict(section).items():
                if key in self.encoding_settings:
                    self.handle_encoding_setting(section, key, value)
                if key in self.overriding_settings:
                    # TODO: delete or set to ''
                    # (in order to override setting in ACTIVE, too)
                    # if self.overriding_settings[key] in section:
                    #     del(section[self.overriding_settings[key]])
                    section[self.overriding_settings[key]] = ''

    def handle_encoding_setting(self, section, key, value):
        """Handle encoding setting with included encoding error handler.

        Drop optional error handler (appended after ':') from value
        and insert at the appropriate setting.
        """
        if ':' in value:
            encoding, handler = value.split(':')
            section[key] = encoding
            section[key+'_error_handler'] = handler

    def collect_active_settings(self):
        # existing active sections
        a_sections = tuple(self[section_name] 
                           for section_name in self.active_sections
                           if section_name in self)
        # collect appending settings
        for section in a_sections:
            for key, value in section.items():
                if key in self.appending_settings:
                    self._appending_setting_lists[key].append(value)
        # update from sections
        for section in a_sections:
            self['ACTIVE'].update(section)
        # update appending settings
        for key, items in self._appending_setting_lists.items():
            delimiter = self.appending_settings[key]
            # strip delimiter, remove duplicates, keep order
            items = (v.strip(delimiter) for v in dict.fromkeys(items))
            # print(key, ':', value)
            self['ACTIVE'][key] = delimiter.join(items)
            
        # remove empty settings
        for section in self.values():
            for key, value in dict(section).items():
                if value == '':
                    del(section[key])


    def optionxform(self, optionstr):
        """
        Lowercase and transform '-' to '_'.

        So the cmdline form of option names can be used in config files.
        """
        return optionstr.lower().replace('-', '_')

cf_parser = ConfigParser()
