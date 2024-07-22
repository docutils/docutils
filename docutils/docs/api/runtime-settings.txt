.. include:: ../header.txt

===========================
 Docutils Runtime Settings
===========================

:Author: David Goodger, Günter Milde
:Contact: docutils-develop@lists.sourceforge.net
:Date: $Date$
:Revision: $Revision$
:Copyright: This document has been placed in the public domain.

.. contents::


Introduction
============

Docutils runtime settings are assembled from several sources:

* Settings specifications of the selected components_,
* `configuration files`_ (if enabled), and
* command-line options (if enabled).

The individual settings are described in `Docutils Configuration`_.


Settings priority
=================

Docutils overlays settings in the following order (later sources
overwrite earlier ones):

1. Defaults specified in the `settings_spec`__ and
   `settings_defaults`__ attributes for each component_.

   __ SettingsSpec.settings_spec_
   __ SettingsSpec.settings_defaults_

2. Defaults specified in the `settings_default_overrides`__ attribute
   for each component_.

   __ SettingsSpec.settings_default_overrides_

3. Application defaults specified in the `settings_overrides`__ argument
   of the `Publisher convenience functions`_.

   __ publisher.html#settings-overrides

4. Settings specified in `active sections`_ of the `configuration files`_
   in the order described in `Configuration File Sections & Entries`_
   (if enabled).

5. Command line options (if enabled).

Applications may opt-out of the standard settings processing providing
their own set of settings.

For details see the ``docutils/__init__.py``, ``docutils/core.py``, and
``docutils.frontend.py`` modules and the implementation description in
`Runtime Settings Processing`_.


.. _SettingsSpec:

SettingsSpec base class
=======================

.. note::
   Implementation details will change with the move to replace the
   deprecated optparse_ module with argparse_.

The `docutils.SettingsSpec` base class is inherited by Docutils
components_ and `frontend.OptionParser`.  It defines six attributes:

attributes
----------

.. _SettingsSpec.settings_spec:

`settings_spec`
   a sequence of

   1. option group title (string or None)

   2. description (string or None)

   3. option tuples with

      a) help text
      b) options string(s)
      c) dictionary with keyword arguments for `OptionParser.add_option()`_
         and an optional "validator", a `frontend.validate_*()` function
         that processes the values (e.g. convert to other data types).

   For examples, see the source of ``frontend.OptionParser.settings_spec``
   or the `settings_spec` attributes of the Docutils components_.

   .. _SettingsSpec.settings_defaults:

`settings_defaults`
   for purely programmatic settings
   (not accessible from command line and configuration files).

   .. _SettingsSpec.settings_default_overrides:

`settings_default_overrides`
   to override defaults for settings
   defined in other components' `setting_specs`.

`relative_path_settings`
   listing settings containing filesystem paths.

`config_section`
   the configuration file section specific to this
   component.

`config_section_dependencies`
   lists configuration files sections
   that should also be read (before the `config_section`).

.. _active sections:

The last two attributes define which configuration file sections are "active".
See also `Configuration File Sections & Entries`_ in the `Docutils
Configuration`_ guide.


Glossary
========

.. _component:

components
----------

Docutils front-ends and applications combine a selection of *components*
of the `Docutils Project Model`_ (reader, parser, writer).

All components inherit the `SettingsSpec`_ base class.
This means that all instances of ``readers.Reader``, ``parsers.Parser``, and
``writers.Writer`` are also instances of ``docutils.SettingsSpec``.

For the determination of runtime settings, ``frontend.OptionParser`` and
applications providing a `SettingsSpec`_ instance via the `settings
specification arguments`_ of the `Publisher convenience functions`_
are treated as components as well.


settings_spec
-------------

The name ``settings_spec`` may refer to

a) an instance of the SettingsSpec_ class,
b) the data structure `SettingsSpec.settings_spec`_ which is used to
   store settings details, or
c) the `"settings_spec" argument`_ of the Publisher convenience functions.


.. References:

.. _Publisher: publisher.html#publisher
.. _Publisher convenience functions:
    publisher.html#publisher-convenience-functions
.. _settings specification arguments: publisher.html#settings-specification
.. _"settings_spec" argument: publisher.html#settings-spec
.. _front-end tools: ../user/tools.html
.. _configuration files:
.. _Docutils Configuration: ../user/config.html#configuration-files
.. _configuration file section:
.. _Configuration File Sections & Entries:
    ../user/config.html#configuration-file-sections-entries
.. _Docutils Project Model: ../peps/pep-0258.html#docutils-project-model
.. _Reader: ../peps/pep-0258.html#reader
.. _Runtime Settings Processing: ../dev/runtime-settings-processing.html

.. _optparse: https://docs.python.org/dev/library/optparse.html
.. _argparse: https://docs.python.org/dev/library/argparse.html
.. _OptionParser.add_option():
    https://docs.python.org/dev/library/optparse.html
    #optparse.OptionParser.add_option
