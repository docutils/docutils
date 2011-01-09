import re, sys, os
import docutils_nest.well_formed_xml

"""


"""

class InlineToXML:
    
    """

    """



    def __init__(   self,
                    start_role = ':',
                    end_role = ':',
                    start_group = '[',
                    end_group = ']',
                    place = 'inside',
                    tag_name = 'inline',
                    warning = 'problematic'
		):
        
        """

        Required:
            
            nothing

        Options:
            
           start_role -- the text delimeter that marks the beginning of a role

           end_role -- the text delimeter that marks the end of a role

           start_group -- the text delimeter that marks the start of a group

           end_group -- the text delimiter that marks the end of a group

           place -- whether the role goes inside or outside of the group

           warning -- a string of text to describe problems

           warning_level -- will do away with this 


        """
        self.__start_role = str(start_role)
        self.__end_role = str(end_role)
        self.__start_group = str(start_group)
        self.__end_group = str(end_group)
        self.__place = str(place)
        exp = self.__make_split_exp()
        self.__split_exp = re.compile(exp)
        exp = self.__role_exp_func()
        self.__start_exp = re.compile(exp)
        self.__token_list = []
        self.__reg_bracket = 0 # found regular brackets
        self.__bracket_type = []
        self.__tag_name, self.__warning = self.__make_tag_names(tag_name, warning)

    def __make_tag_names(self, tag_name, warning):
        tags = [tag_name, warning]
        counter = 0
        for name in tags:
            new_string = ''
            counter += 1
            for char in name:
                if not char.isalnum():
                    char = '_'
                new_string += char
            first = new_string[:1]
            is_letter = first.isalpha()
            if not is_letter:
                new_string = 'a' + new_string
            if counter == 1:
                new_tag_name = new_string
            else:
                new_warning = new_string
        return new_tag_name, new_warning

    def __escape_value(self, value):

        """

        Required:

            value -- a string

        Returns

            a string of escaped values

        Logic:

            Certain characters have special values in regular expressions. If
            the user is using any of these special expressions, then they must
            be escaped.


        """
        special = ['[', ']', '(', ')', '{', '}', '.']
        if value in special:
            value = '\\%s' % value
        return value
    
    def __make_split_exp(self):

        """

        Requires:

            nothing

        Returns:

            nothing

        Logic:

            Use the object values to form a regular expression that will be
            used to split a string into tokens. The expression will be
            different if the role is inside or outside the group.


        """
        start_group = self.__escape_value(self.__start_group)
        end_group = self.__escape_value(self.__end_group)
        start_role = self.__escape_value(self.__start_role)
        end_role = self.__escape_value(self.__end_role)
        if self.__place == 'inside':
		  #\[:[^\[\]]+:\s?|\[|\]
            exp = '(%s%s[^%s%s]+%s\s?|%s|%s)' % \
			  (start_group,
			   start_role,
			   start_group,
			   end_group,
			   end_role,
			   start_group,
			   end_group
			   )
			   
		  		
        else:
		  # :[^\[\]]+:\[|[|]
	       exp = '(%s[^%s%s]+%s%s|%s|%s)' %\
		      (start_role,
			 start_group,
			 end_group,
			 end_role,
			 start_group,
			 start_group,
			 end_group
			 )
        return exp

    def __role_exp_func(self):
        """

        Requires:

            nothing

        Returns

            nothing

        Logic:

            Make a regular expression to marke the beginning of a group

        """
        start_group = self.__escape_value(self.__start_group)
        end_group = self.__escape_value(self.__end_group)
        start_role = self.__escape_value(self.__start_role)
        end_role = self.__escape_value(self.__end_role)
        if self.__place == 'inside':
            exp = '%s%s(.*?)%s' % \
             (start_group,
             start_role,  
             end_role
             )
        else:
            exp = '%s(.*?)%s%s' % \
             (start_role,
             end_role,  
             start_group
             )
        return exp



    def __well_formed_func(self, my_string):
        """

        Required:

            my_string --string to check

        Returns:

            0 if the string can be convert to tags, and 0 otherwise

        Logic:

            Use the external module well_formed_xml to test that the tags are
            balanced. If they are balanced, the module returns 1. Othewise it
            returns 0


            """
        well_formed_obj = docutils_nest.well_formed_xml.WellFormed()
        well_formed = well_formed_obj.well_formed(my_string)
        return well_formed



    def __open_tag(self, my_string):
        """

        Requires:

            my_string --string to parse

        Returns:

            A string of arguments for an opening tag.

        Logic:

            Use a simple split expression to turn a string of values with a
                space bewteen them into arguments: word1 word2 word3 =>
                arg1="word1" arg2="word2" arg3="word3"

        """
        arguments = my_string.split()
        counter = 0
        final_string = ''
        for argument in arguments:
            counter += 1
            final_string += 'arg%s="%s" ' % (counter, argument)
        final_string = final_string.replace(':', '')
        return final_string[:-1] # chop off last space



    def __process_inside_group(self, inner_list):
        """

        Requires:

            inner_list -- a list of two values. The first is a string starting
            and ending with a colon. :arg1 arg2:

        Returns:

            A string of tagged text. If the string can be converted to XML (or
                tagged), then it is tagged:
                    
                    [':word1 word2:', 'text'] => <inline arg1="word1"
                    arg2="word2">text</inline>

            If the string cannot be tagged, it makes the following:

                [':word1 word2:', 'text<i>'] => <warning descrition=
                description="tagging brackets will result in invalid XML'

        Logic:

            Check to see if you can tag the text and still form valid XML. If
            so, tag and return the text. 

            Otherwise, write an error message and return the text.



        """
        my_string = inner_list[1]
        tag = inner_list[0]
        well_formed = self.__well_formed_func(my_string)
        if well_formed:
            match_obj = re.match(self.__start_exp, tag)
            tag = match_obj.group(1)
            open_tag = self.__open_tag(tag) #[:wor1: 
            final_string = '<%s %s>%s</%s>' % (
                          self.__tag_name, open_tag, my_string, self.__tag_name)
            return final_string
        else:
            final_string = '<%s description="tagging brackets will result ' % (self.__warning,
                             )
            final_string += 'in invalid XML"/>'
            final_string +=  tag + my_string + ']'
            warning = (
            'WARNING: tagging brackets will result in invalid XML:'
            '%s%s' % (tag, my_string)
            )
            sys.stderr.write(repr(warning))
            sys.stderr.write('\n')
            return final_string

    def __start_role_func(self, token):
        """
        Requires:
         
            token -- the token to check

        Returns:

            A string if a start expession is found; 0 otherwise

        """

        match_obj = re.match(self.__start_exp, token)
        
        if match_obj:
            return match_obj.group(1)
        return 0
    
    def __tag(self, tokens):
        """

        Requires:

            tokens -- a list of tokens

        Returns:

            A string --A string of tagged text

        Logic:

            Read one token at a time.

            If the token is text, add it to the final string. 'Text' => 'text'

            If the token is an opening bracket and a colon, make a new list =>
            [:word1: text] => [[:word1:, text]]

            If the token is a simple opening bracket, simply set the regular
            bracket tag to true.

            If the token is a closed bracket, and the regular bracket is not
            true, get the last item in the group. ([:word1: text]). Process
            this list and get a string back. If there the list still has
            items, add it to the text part: 

                returned string = <inline arg1="word1"> text</inline>

                list = [[:arg2:, "more text]]

                Becomes:

                    list = [[:arg2:, 'more text <inline arg1="word1" text </inline>']

            Otherwise, add the token either to the final string, or to the
            text portion of the list. 

        """
        final_string = ''
        for token in tokens:
            start_role = self.__start_role_func(token)
            ##print repr(token)
            ##if token == ']' and not self.__reg_bracket:
            if token == self.__end_group and self.__bracket_type and \
                self.__bracket_type[-1] == 'inline':

                inside_string = self.__token_list.pop()
                parsed_string = self.__process_inside_group(inside_string)
                if self.__token_list:
                    self.__token_list[-1][1] += parsed_string
                else:
                    final_string += parsed_string

                self.__bracket_type.pop()

            # check for solitary closing brackets
            elif token == self.__end_group and not self.__bracket_type:
                sys.stderr.write('ending bracket may cause unwanted results\n')
                if self.__token_list:
                    self.__token_list[-1][1] += token
                else:
                    final_string += token
            elif start_role:
                self.__token_list.append([])
                self.__token_list[-1].append(token)
                self.__bracket_type.append('inline') # inline bracket group
                self.__token_list[-1].append('')
            else:
                # first test if you should add or pop list type
                if token == self.__end_group:
                    ##self.__bracket_type.pop()
                    try:
                        self.__bracket_type.pop()
                    except IndexError:
                        pass
                elif token[0:1] == self.__start_group:
                    self.__bracket_type.append('regular') # regular bracket group

                # now add the string to the right place
                if self.__token_list:
                    self.__token_list[-1][1] += token # add to middle group
                else:
                    final_string += token
        if self.__token_list:
            warning = 'WARNING Brackets not closed out\n'
            warning +=  self.__token_list[0][0] + self.__token_list[0][1]
            sys.stderr.write(repr(warning))
            warning = '<%s description="Brackets not closed properly"/>' % (
                      self.__warning)
            final_string += warning + self.__token_list[0][0] + self.__token_list[0][1]
            self.__token_list = []

        return final_string

    def make_tags(self, my_string):
        my_list = re.split(self.__split_exp, my_string)
        my_string = self.__tag(my_list)
        return my_string



if __name__ == '__main__':
    start_role = ':'
    end_role = ':'
    start_group = '['
    end_group = ']'
    place = 'inside'

    test_obj = InlineToXML(
     start_role = start_role,
	end_role = end_role,
	place = place,
	start_group = start_group,
	end_group = end_group
	)
    the_test_string = '<i role = "arg">[:role: test22 [:role2: text2]</i>][]'
    return_string = test_obj.make_tags(the_test_string)
    print '@%s@' % return_string

    start_role = ':'
    end_role = ':'
    start_group = '['
    end_group = ']'
    place = 'outside'

    test_obj = InlineToXML(
     start_role = start_role,
	end_role = end_role,
	place = place,
	start_group = start_group,
	end_group = end_group
	)

    the_test_string = ':role:[ test :role2:[ text2]]'
    return_string = test_obj.make_tags(the_test_string)
    print return_string
