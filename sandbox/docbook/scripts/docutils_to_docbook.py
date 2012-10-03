import os, sys, subprocess, argparse, tempfile, logging, glob
import asciitomathml.asciitomathml 
import validate_docbook, validate_fo
import fop
import logging
import xsl_process
import lxml
from lxml import etree as etree

try:
    import locale
    locale.setlocale(locale.LC_ALL, '')
except:
    pass

class NoRunException(Exception):
    pass

class ToXml():
    """
    convert to XML with math string

    """

    def __init__(self, in_file, in_encoding='utf8', to_docbook=True, 
            validate_docbook = True, convert_to_fo = True, convert_to_pdf = True, debug=False):
        self.path_id = '__rst__'
        self._transform_num = 0
        self.in_file = in_file
        self.in_encoding = in_encoding
        self.validate_docbook = validate_docbook
        self.convert_to_fo = convert_to_fo
        self.convert_to_pdf = convert_to_pdf
        self.debug = debug
        self.make_logging()
        self.debug=True

    def make_logging(self, ch_level=logging.ERROR, fh_level=logging.INFO):
        logger = logging.getLogger(__name__)
        logger.setLevel(logging.DEBUG)
        fh = logging.FileHandler('docutils_to_xml.log')
        fh.setLevel(fh_level)
        ch = logging.StreamHandler()
        ch.setLevel(ch_level)
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        ch.setFormatter(formatter)
        fh.setFormatter(formatter)
        logger.addHandler(ch)
        logger.addHandler(fh)
        self.logger = logger

    def pretty_print(self, elem, level=0):
        i = "\n" + level*"  "
        if len(elem):
            if not elem.text or not elem.text.strip():
                elem.text = i + "  "
            if not elem.tail or not elem.tail.strip():
                elem.tail = i
            for elem in elem:
                self.pretty_print(elem, level+1)
            if not elem.tail or not elem.tail.strip():
                elem.tail = i
        else:
            if level and (not elem.tail or not elem.tail.strip()):
                elem.tail = i

    def rst_to_xml(self, in_file, base=None, in_encoding='utf8'):
        from docutils.core import publish_cmdline, default_description
        input_encode = '--input-encoding={0}'.format(in_encoding)
        output_encode = '--output-encoding=ascii'
        needed_opts = ['--traceback', '--strip-comments', '--trim-footnote-reference-space', 
                '--no-doctype', input_encode, output_encode, '--no-generator', ]
        fh, temp_file = tempfile.mkstemp()
        if not base:
            base = [sys.argv[0]]
        sys.argv = base +  needed_opts + [in_file]
        with open(temp_file, 'w') as write_obj:
            stdout = sys.stdout
            sys.stdout = write_obj
            description = ('Generates Docutils-native XML from standalone '
                           'reStructuredText sources.  ' + default_description)
            publish_cmdline(writer_name='xml', description=description)
            sys.stdout = stdout
        with open(temp_file, 'r', ) as read_obj:
            xml_string = ''.join(read_obj.readlines())
        os.close(fh)
        os.remove(temp_file)
        return xml_string

    def insert_math_elements(self, xml_string, out_encoding='ASCII'):
        tree = etree.XML(xml_string)
        for ma in ['math_block', 'math']:
            for e in tree.iter(ma):
                math_text = e.text
                e.text = ''
                if ma == 'math_block':
                    math_obj =  asciitomathml.asciitomathml.AsciiMathML(mstyle={'displaystyle':'true'})
                else:
                    math_obj =  asciitomathml.asciitomathml.AsciiMathML()
                math_obj.parse_string(math_text)
                math_tree = math_obj.get_tree()
                math_string = etree.tostring(math_tree, encoding='ascii') 
                math_tree = etree.XML(math_string)
                e.insert(0, math_tree)
        xml_string = etree.tostring(tree, encoding=out_encoding)
        return xml_string

    def _make_temp(self, the_type):
        filename, ext = os.path.splitext(self.in_file)
        basename = os.path.basename(self.in_file)
        if the_type == 'transform':
            self._transform_num += 1
            return '{0}{1}transform{2}.xml'.format(filename, self.path_id, self._transform_num ) 
        elif the_type == 'docbook':
            return '{0}{1}docbook.xml'.format(filename, self.path_id ) 
        elif the_type == 'rst':
            return  '{0}{1}raw.xml'.format(filename, self.path_id ) 
        elif the_type == 'fo':
            return '{0}{1}docbook.fo'.format(filename, self.path_id ) 

    def to_docbook(self, raw_path, xsl_files = []):
        doc_home = os.environ.get('RST_DOCBOOK_HOME')
        if not doc_home:
            raise OSError('You must set RST_DOCBOOK_HOME')
        if len(xsl_files) == 0:
            xsl_file = os.path.join(doc_home, 'docutils_to_docbook.xsl')
            if not os.path.isfile(xsl_file):
                raise IOError('cannot find "{0}'.format(xsl_file))
            xsl_files = [xsl_file]
        in_files = [raw_path]
        counter = 0
        for xsl in xsl_files:
            if counter + 1 == len(xsl_files):
                the_type = 'docbook'
            else:
                the_type = 'transform'
            out_file = self._make_temp(the_type=the_type)
            self.logger.debug('out file from temp is "{0}"'.format(out_file))
            error, xml_obj = xsl_process.transform_lxml(xsl, in_files[counter])
            in_files.append(out_file)
            counter += 1
        return xml_obj

    def to_fo(self, result_tree_obj, xsl_files = []):
        doc_home = os.environ.get('DOCBOOK_HOME')
        if not doc_home:
            raise OSError('You must set DOCBOOK_HOME')
        if len(xsl_files) == 0:
            xsl_file = os.path.join(doc_home, 'fo', 'docbook.xsl')
            if not os.path.isfile(xsl_file):
                raise IOError('cannot find "{0}'.format(xsl_file))
            xsl_files = [xsl_file]
        in_files = [result_tree_obj]
        counter = 0
        for xsl in xsl_files:
            if counter + 1 == len(xsl_files):
                the_type = 'fo'
            else:
                the_type = 'transform'
            out_file = self._make_temp(the_type=the_type)
            self.logger.debug('out file from temp is "{0}"'.format(out_file))
            error, xml_obj = xsl_process.transform_lxml(xsl, in_files[counter])
            in_files.append(out_file)
            counter += 1
        return xml_obj


    def to_pdf(self, fo_file):
        fop_obj = fop.Fop()
        pdf_file = fop_obj.to_pdf(fo_file)

    def report_xsl_error(self, transform_error_obj):
        for error_obj in transform_error_obj:
            sys.stderr.write(error_obj.message)
            sys.stderr.write('\n')
            if error_obj.line != 0 and error_obj.column != 0:
                self.logger.critical(str(error_obj.line))
                self.logger.critical(str(error_obj.column))

    def validate_docutils_rng(self, xml_obj):
        valid_home = os.environ.get('VALIDATE_HOME')
        if valid_home == None:
            raise IOError('You need to set the variable "VALIDATE_HOME"')
        docbook_rng = os.path.join(valid_home, 'relax', 'docbook.rng')
        if not os.path.isfile(docbook_rng):
            msg = 'cannot find "{0}"'.format(docbook_rng)
            msg += '\nYou need download docbook.rng'
            raise IOError(msg)
        # the_rng = os.path.join(os.path.dirname(__file__), 'valid','docutils.rng') 
        relaxng_doc = etree.parse(open(docbook_rng, 'r'))
        relaxng = etree.RelaxNG(relaxng_doc)
        is_valid = relaxng.validate(xml_obj)
        if not is_valid:
            self.logger.critical('Not valid docbook\n')
            self.report_xsl_error(relaxng.error_log)
            return 1

    def validate_fo_xsl(self, result_tree_obj):
        # xsl_ss = os.path.join(os.path.dirname(__file__), 'valid','folint.xsl')
        valid_home = os.environ.get('VALIDATE_HOME')
        if valid_home == None:
            raise IOError('You need to set the variable "VALIDATE_HOME"')
        xsl_ss = os.path.join(valid_home, 'xslt', 'folint.xsl')
        if not os.path.isfile(xsl_ss):
            msg = 'cannot find "{0}"'.format(docbook_rng)
            msg += '\nYou need download docbook.rng'
            raise IOError(msg)
        xslt_doc = etree.parse(xsl_ss) 
        transform = etree.XSLT(xslt_doc)
        indoc = result_tree_obj
        try:
            outdoc = transform(indoc)
        except lxml.etree.XSLTApplyError, error:
            msg = 'error converting %s to %s with %s:\n' % (xml_file, out_file, xslt_file)
            msg += str(error)
            msg += '\n'
            self.logger.critical(msg)
            self.report_xsl_error(transform.error_log)
            return 1
        self.report_xsl_error(transform.error_log)
        return  len(transform.error_log)


    def clean(self, the_dir):
        pattern = os.path.join(the_dir, '*{0}docbook.xml'.format(self.path_id))
        files = glob.glob(pattern)
        pattern = os.path.join(the_dir, '*{0}transform[0-9].xml'.format(self.path_id))
        files += glob.glob(pattern)
        pattern = os.path.join(the_dir, '*{0}raw.xml'.format(self.path_id))
        files += glob.glob(pattern)
        self.logger.debug(files)
        for f in files:
            os.remove(f)


    def convert(self):
        xml_string = self.rst_to_xml(in_file=self.in_file, in_encoding=self.in_encoding)
        xml_string = self.insert_math_elements(xml_string)
        raw_path = self._make_temp(the_type = 'rst')
        with open(raw_path, 'w') as write_obj:
            write_obj.write(xml_string)
        result_obj = self.to_docbook(raw_path)
        if self.validate_docbook:
            valid = self.validate_docutils_rng(result_obj)
        if self.convert_to_fo:
            result_obj = self.to_fo(result_obj)
            valid = self.validate_fo_xsl(result_obj)
            fo_file = self._make_temp(the_type = 'fo')
            if self.debug:
                root = etree.tostring(result_obj, pretty_print=True)
            else:
                root = etree.tostring(result_obj)
            with open(fo_file, 'w') as write_obj:
                write_obj.write(root)
            if self.convert_to_pdf:
                self.to_pdf(fo_file)



if __name__ == '__main__':
    to_xml_obj = ToXml(sys.argv[1])
    to_xml_obj.convert()
