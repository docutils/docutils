import xml.parsers.expat

"""

well_formed module
^^^^^^^^^^^^^^^^^^

========
Overview
========

Takes a string, and returns either a zero or one, depending if the string has
balanced tags.


"""

class WellFormed:

    def __init__(self):
        pass

    def well_formed(self, my_string):
        """

        Requires:

            my_string --string to test for well-formedness

        Returns:

            1, if the tags are balanced, and 0 if the tags are not

        Logic:

            Enclose the string around an artifical root element. 

            Change the encoding, if necessary.

            Try to parse the string with expat. If there is success, return 1.
            Otherwise, catch the exception and return 0.

        """
        enclosing_tag = 'doc-string-test6685866'
        my_string = '<%s>%s</%s>' % (enclosing_tag, my_string, enclosing_tag)
        try:
            if isinstance(my_string, type(u"")):
                my_string = my_string.encode("utf-8")
            parser = xml.parsers.expat.ParserCreate()
            parser.Parse(my_string, 1)
            return 1
        except xml.parsers.expat.ExpatError:
            return 0      


if __name__ == '__main__':
    test_strings = [
    '<tag>simplest test</tag>',
    '<tag>simplest fail test<tag>',
    '<tag>&amp;text\u201c</tag><tag>text,</tag>',
    '<tag>&amp;text&lt;</tag><tag>text,</tag>',
    '</tag>&amp;text&lt;</tag><tag>text,<tag>',
    ]
    test_obj = WellFormed()
    for test_string in test_strings:
        well_formed = test_obj.well_formed(test_string)
        if well_formed:
            print '%s is well formed' % test_string
        else:
            print '%s is not well formed' % test_string



