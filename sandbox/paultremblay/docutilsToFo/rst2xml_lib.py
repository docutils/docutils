#!/Library/Frameworks/Python.framework/Versions/2.7/Resources/Python.app/Contents/MacOS/Python
# $Id$ 

import docutils.core
import cStringIO, sys
from docutils.core import  default_description


the_settings_dict = {   '_config_files': ['/Users/cynthia/.docutils'],
    '_destination': None,
    '_disable_config': None,
    '_source': 'test.rst',
    'auto_id_prefix': 'id',
    'config': None,
    'datestamp': None,
    'debug': None,
    'docinfo_xform': 1,
    'doctitle_xform': 1,
    'doctype_declaration': 1,
    'dump_internals': None,
    'dump_pseudo_xml': None,
    'dump_settings': None,
    'dump_transforms': None,
    'error_encoding': 'US-ASCII',
    'error_encoding_error_handler': 'backslashreplace',
    'exit_status_level': 5,
    'expose_internals': None,
    'file_insertion_enabled': 1,
    'footnote_backlinks': 1,
    'generator': None,
    'halt_level': 4,
    'id_prefix': '',
    'indents': None,
    'input_encoding': None,
    'input_encoding_error_handler': 'strict',
    'language_code': 'en',
    'newlines': None,
    'output_encoding': 'utf-8',
    'output_encoding_error_handler': 'xmlcharrefreplace',
    'pep_base_url': 'http://www.python.org/dev/peps/',
    'pep_file_url_template': 'pep-%04d',
    'pep_references': None,
    'raw_enabled': 1,
    'report_level': 2,
    'rfc_base_url': 'http://www.faqs.org/rfcs/',
    'rfc_references': None,
    'sectnum_xform': 1,
    'sectsubtitle_xform': 0,
    'source_link': None,
    'source_url': None,
    'strict_visitor': None,
    'strip_classes': None,
    'strip_comments': None,
    'strip_elements_with_classes': None,
    'tab_width': 8,
    'title': 'BOZO',
    'toc_backlinks': 'entry',
    'traceback': None,
    'trim_footnote_reference_space': None,
    'warning_stream': None,
    'xml_declaration': 1}

class XMLPublish(docutils.core.Publisher):


    default_usage = '%prog [options] [<source> [<destination>]]'
    default_description = ('Reads from <source> (default is stdin) and writes to '
                       '<destination> (default is stdout).  See '
                       '<http://docutils.sf.net/docs/user/config.html> for '
                       'the full reference.')

    description = ('Generates Docutils-native XML from standalone '
                   'reStructuredText sources.  ' + default_description)

    # doesn't work, and not sure why
    def __init_old__(self):
         docutils.core.Publisher.__init__(self)

    def publish(self, argv=None, usage=None, description=None,
                settings_spec=None, settings_overrides=None,
                config_section=None, enable_exit_status=None):
        """
        Process command line options and arguments (if `self.settings` not
        already set), run `self.reader` and then `self.writer`.  Return
        `self.writer`'s output.
        """
        default_for_xml = {'trim_footnote_reference_space': True} 
        if settings_overrides == None:
            settings_overrides = {}
        settings_overrides.update(default_for_xml)
        exit = None
        try:
            if self.settings is None:
                self.process_command_line(
                    argv, usage, description, settings_spec, config_section,
                    **(settings_overrides or {}))
            self.set_io()

            # had to add  this as well
            the_source_path = settings_overrides.get('_source')
            if the_source_path:
                source_path = the_source_path
                source = file(the_source_path, 'rU')
                self.source = self.source_class(
                    source=source, source_path=source_path,
                    encoding=self.settings.input_encoding)
            self.document = self.reader.read(self.source, self.parser,
                                             self.settings)
            self.apply_transforms()
            # had to add the destination_path and destination to redirect
            # output
            destination_path = settings_overrides.get('_destination')
            if not destination_path:
                output = cStringIO.StringIO()
                destination = output
            else:
                destination = None
            self.destination = self.destination_class(
                destination=destination, destination_path=destination_path,
                encoding=self.settings.output_encoding,
                error_handler=self.settings.output_encoding_error_handler)
            output = self.writer.write(self.document, self.destination)
            self.writer.assemble_parts()
        except SystemExit, error:
            exit = 1
            exit_status = error.code
        except Exception, error:
            if not self.settings:       # exception too early to report nicely
                raise
            if self.settings.traceback: # Propagate exceptions?
                self.debugging_dumps()
                raise
            self.report_Exception(error)
            exit = 1
            exit_status = 1
        self.debugging_dumps()
        if (enable_exit_status and self.document
            and (self.document.reporter.max_level
                 >= self.settings.exit_status_level)):
            sys.exit(self.document.reporter.max_level + 10)
        elif exit:
            sys.exit(exit_status)
        return output


    def publish_xml_cmdline(self, source = None, destination = None, 
                        reader=None, reader_name='standalone',
                        parser=None, parser_name='restructuredtext',
                        writer=None, writer_name='pseudoxml',
                        settings=None, settings_spec=None,
                        settings_overrides=None, config_section=None,
                        enable_exit_status=1, argv=None,
                        usage=None, description=None):
        """
        Set up & run a `Publisher` for command-line-based file I/O (input and
        output file paths taken automatically from the command line).  Return the
        encoded string output also.

        Parameters: see `publish_programmatically` for the remainder.

        - `argv`: Command-line argument list to use instead of ``sys.argv[1:]``.
        - `usage`: Usage string, output if there's a problem parsing the command
          line.
        - `description`: Program description, output for the "--help" option
          (along with command-line option descriptions).
        """
        if usage == None:
            usage = self.default_usage
        if description == None:
            description = self.description

        if source:
            if settings_overrides == None:
                settings_overrides = {}
            settings_overrides['_source'] = source
        if destination:
            if settings_overrides == None:
                settings_overrides = {}
            settings_overrides['_destination'] = destination

        pub = XMLPublish(reader, parser, writer, settings=settings)
        pub.set_components(reader_name, parser_name, writer_name)
        output = pub.publish(
            argv, usage, description, settings_spec, settings_overrides,
            config_section=config_section, enable_exit_status=enable_exit_status)
        return output


"""

from docutils.core import  default_description



"""

if __name__ == '__main__':
    try:
        import locale
        locale.setlocale(locale.LC_ALL, '')
    except:
        pass
    xml_obj = XMLPublish()
    the_out = xml_obj.publish_xml_cmdline(writer_name='xml')

    print the_out
