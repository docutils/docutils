import os, re, sys, codecs

import xml.sax.saxutils
import xml.sax

# turn on this line if you want to disablenamespaces
##from xml.sax.handler import feature_namespaces

##import paul.restructure_tools.handle_inline_text
import docutils_nest.inline_to_xml
# # import txt_to_xml.brackets_to_xml

"""

Inline 
^^^^^^

:author: Paul Tremblay

:revision:

  :revnumber: .2

  :date: 2003-05-03

  :revremark: When I first started writing this document.

  :revnumber: .21

  :date: 2003-05-06

  :revremark: Turned off namespaces. Tried "to   get rid of using import ..
              from" construction, since I don't understand it.

  :revnumber: .22

  :date: 2003-05-09

  :revremark: Documented the module. Escaped brackets such as \[. Escaped any
              bracket in an attribute.

  :revnumber: .23

  :date: 2003-05-31

  :revremark: The user can now choose his or her own way to define groups.

========
Overview
========

This module gets a file and changes brackets to inline tags:

<document>

  <paragraph>Text [:word1 word2 word3: Text [:word1 word2: Text2 [regular bracket]]Text3]
  </paragraph>

</document>


<document>

  <paragraph>Text <arg1="word1" arg2="word2" arg3="word3"> Text <inline arg1="word1" arg2= "word2"> Text2 [regular bracket]</inline>Text3</inline>
  </paragraph>

</document>

"""




class InlineHandler(xml.sax.saxutils.DefaultHandler):
    """

    Class for handling the XML file. SAX uses the methods in this class to
    handle the data. 


    """
    def __init__(   self,  
                    write_obj,
                    start_role = ':',
                    end_role = ':',
                    start_group = '[',
                    end_group = ']',
                    place = 'inside',
                    tag_name = 'inline',
                    warning = 'problematic'
                
                ):

        """
        

        Requires:

            write_obj -- a write object

        Returns:

            nothing


        Logic:

            Set the necessary parameters:

                self.__write_obj --the object for outputing text

                self.__name --name of current element

                self.__character --string containg characters in current
                element.

                self.__in_block --whether the text is in a block element.
                
                self.__block_tags -- a list of tags that include blocks of
                text. In other words, all tags except those tags that surround
                inline items.

                self.__block_tags -- the string of all text and elements in
                the current block.

                self.__handl_br_text_obj -- an object to handle all block text
                that contains an open bracket.

        """
        self.__write_obj = write_obj
        self.__name = ''
        self.__character = ''
        self.__in_block = 0
        self.__block_tags = ['paragraph', 'author', 'date', 'revision',
        'version']
        self.__block_tag_string = ''
        # self.__handle_br_text_obj = \
        # rst_bracket_inline.handle_bracket_string.Inline()
        self.__handle_br_text_obj = \
            docutils_nest.inline_to_xml.InlineToXML(
                                                    start_role = start_role,
                                                    end_role = end_role,
                                                    start_group = start_group,
                                                    end_group = end_group,
                                                    place = place,
                                                    tag_name = tag_name,
                                                    warning = warning
                                                    )
    
    def startElement(self, name, attrs):
        """

        Logic:

            The SAX driver uses this function when if finds a beginning tag.

            Escape all opening and closing brackets for the values of
            attributes, so they won't be processed. 

            Make a string from the opening tag. If you are in a block element,
            add this string to the block element string. Otherwise, write the
            string to the file.



        """
        self.__name = name
        if name in self.__block_tags:
            self.__in_block = 1
        open_tag = '<%s' % name
        keys = attrs.keys()
        for key in keys:
            att = key
            value = attrs[key]
            # kind of a kludge. Only escape text that is going to be unescaped
            # later on
            if self.__in_block:
                value = value.replace('[', '\000')
                value = value.replace(']', '\001')
            open_tag += ' %s="%s"' % (att, value)
        open_tag += '>'
        if self.__in_block:
            self.__block_tag_string += open_tag
        else:
            self.__write_obj.write(open_tag)

    
    def characters(self, character):
        """

        Logic:

            The SAX driver uses this function when it finds text.

            If the text is between two literal tags, then I want to escape all
            opening and closing brackts to that they are not processed.

            If the text is not between two literal tags, I want to escape a
            backlash followed by a bracket.

            (Note: The only way that a file processed by docutils-xml will
            pass on a backslash followed by a bracket is if the original file
            has *2* backslashes followed by a bracket.)

            Add the changed string to the block test string.

            If the text is not in a block (which I don't think should happen),
            it simply outputs it to the file.

        """

        character = character.replace('&', '&amp')
        character = character.replace('<', '&lt;')
        character = character.replace('>', '&gt;')
        if self.__in_block:
            if self.__name == 'literal':
                character = character.replace('[', '\000')
                character = character.replace(']', '\001')
            # replace escaped backslashes not in literal
            else:
                character = character.replace('\\]', '\001')
            self.__block_tag_string += character
        else:
            self.__write_obj.write(character)
            
    
    def endElement(self, name):
        """

        Logic:

            The SAX driver uses the function when it finds an end tag. It
            pases to this function the name of the end element.

            If the name is a block element, the function checks if it has any
            starting brackets. If it does, the string should be processed with
            the handle bracket module.

            The text that has been escaped is not unescaped, and the string is
            written to the output file.

            If the tag does not indicate the end of a block, but you are in a
            block tag, add the text to the block string.

            If the text has nothing to do with a block, simly output it to the
            file.


        """


        if name in self.__block_tags:
            self.__block_tag_string += '</%s>' % name
            # handle all the text
            if '[' in self.__block_tag_string:
                tagged_text = \
                 self.__handle_br_text_obj.make_tags(self.__block_tag_string)
            else:
                tagged_text = self.__block_tag_string
            tagged_text = tagged_text.replace('\000', '[')
            tagged_text = tagged_text.replace('\001', ']')
            self.__write_obj.write(tagged_text)
            self.__in_block = 0
            self.__block_tag_string = ''
        elif self.__in_block:
            self.__block_tag_string += '</%s>' % name
        else:
            self.__write_obj.write('</%s>' % name)




class InlineBrackets:

    def __init__(   self, 
                    file, 
                    output,
                    start_role = ':',
                    end_role = ':',
                    start_group = '[',
                    end_group = ']',
                    place = 'inside',
                    tag_name = 'inline',
                    warning = 'problematic'
                    
                    ):


        """
        

        Requires:

            file --file to be read

            output --file to output to

        
        Returns:

            Nothing. Outputs a file

        Logic:

            Set up a write object. 

            Create an instance of the InlineHandler for sax to use.

            Pass this instance to the SAX driver.

            Use the SAX driver to handle the file. 


        """
        self.__output = output 
        self.__file = file
        self.__start_role = start_role
        self.__end_role = end_role
        self.__start_group = start_group
        self.__end_group = end_group
        self.__place = place
        self.__tag_name = tag_name
        self.__warning = warning

    def make_tags(self):
        (utf8_encode, utf8_decode, utf8_reader, utf8_writer) = codecs.lookup("utf-8")
        write_obj = utf8_writer(open(self.__output, 'w'))
        parser = xml.sax.make_parser()
        # turn on this line if you want to disable namespaces
        ##parser.setFeature(feature_namespaces, 0)
        inline_handler = InlineHandler( write_obj = write_obj,
                                        start_role = self.__start_role,
                                        end_role = self.__end_role,
                                        start_group = self.__start_group,
                                        end_group = self.__end_group,
                                        place = self.__place,
                                        tag_name = self.__tag_name,
                                        warning = self.__warning
                                        )
        parser.setContentHandler(inline_handler)
        parser.parse(self.__file)             
        write_obj.close()


            
if __name__ == '__main__':
    file = '/home/paul/lib/python/paul/restructure_tools/test_inline.xml'
    output = '/home/paul/paultemp/brackets_to_tags.temp.xml'
    obj = InlineBrackets(file, output = output, warning = '5fuck:? yo&<>u')
    obj.make_tags()
    command = 'xmlvalid -c -v %s' % output
    os.system(command)
