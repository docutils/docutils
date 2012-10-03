from lxml import etree
import lxml
import os, sys, logging

def report_xsl_error(transform_error_obj):
    for error_obj in transform_error_obj:
        sys.stderr.write(error_obj.message)
        sys.stderr.write('\n')
        if error_obj.line != 0 and error_obj.column != 0:
            sys.stderr.write(str(error_obj.line))
            sys.stderr.write(str(error_obj.column))

def transform_lxml(xslt_file, xml_file,  param_dict = {}):
    # have to put quotes around string params
    temp = {}
    the_keys = param_dict.keys()
    for the_key in the_keys:
        if len(the_key) > 0: 
            if the_key[0] == '"' and the_key[-1] == '"':
                temp[the_key] = param_dict[the_key]
            elif the_key[0] == "'" and the_key[-1] == "'":
                temp[the_key] = param_dict[the_key]
            else:
                temp[the_key] = "'{0}'".format(param_dict[the_key])
    param_dict = {}
    param_dict.update(temp)

    try:
        xslt_doc = etree.parse(xslt_file)
    except lxml.etree.XMLSyntaxError as error:
        sys.stderr.write(str(error))
        return 1, None
    try:
        transform = etree.XSLT(xslt_doc)
    except lxml.etree.XSLTParseError as error:
        sys.stderr.write(str(error) + '\n')
        return 1, None

    if not isinstance(xml_file, lxml.etree._XSLTResultTree):
        try:
            indoc = etree.parse(xml_file)
        except lxml.etree.XMLSyntaxError as msg:
            sys.stderr.write('Invalid XML\n')
            sys.stderr.write(str(msg))
            sys.stderr.write('\n')
            return 1, None
    else:
        indoc = xml_file
    try:
        outdoc = transform(indoc, **param_dict)
    except lxml.etree.XSLTApplyError as error:
        msg = 'error converting %s to %s with %s:\n' % (xml_file, out_file, xslt_file)
        msg += str(error)
        msg += '\n'
        report_xsl_error(transform.error_log)
        return 1, None, None
    report_xsl_error(transform.error_log)
    return 0, outdoc
if __name__ == '__main__':
    error, doc_obj = transform_lxml(xslt_file = sys.argv[1], xml_file= sys.argv[2]) 
    if not error:
        doc_obj.write('foo', encoding='utf8')
