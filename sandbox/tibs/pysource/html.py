"""Output DOCUTILS nodes as HTML.

This is a quick-and-dirty approach to writing out HTML derived from
a DOCUTILS node tree. It maintains a minimum of state, and doesn't attempt
any particular intelligence about the tree structure.

    Note: for debugging purposes some HTML elements are output with
    "style" attributes - this is so I can track which elements were
    written for what purpose, and is temporary.

    (Use of "class" attributes to make CSS usage easier is a
    separate consideration, to be made later on.)

Use of this should ultimately be replaced by use of David's new mechanisms
from the docutils module - but they didn't exist when I started, so we'll live
with this for a little longer.
"""

import time
import buildhtml

__docformat__ = "reST"

class HTMLError(Exception):
    pass


# ----------------------------------------------------------------------
class Writer:
    """Encapsulate the HTML writing stuff in a class

    - it makes it easier to handle values we want to keep around
    """

    colours = {"Information": "#FF0000",
               "Warning"    : "#FF0000",
               "Error"      : "#FF0000",
               "Fatal"      : "#FF0000",
               "WarningBG"  : "Silver",   # (was "lightgrey" or #DDDDDD)
               "default"    : "#FFFFCC",
               }
    """Colours to use to distinguish various contexts

    Note that the HTML4 spec defines the following colours:

    * Black   = "#000000"
    * Silver  = "#C0C0C0"
    * Gray    = "#808080"
    * White   = "#FFFFFF"
    * Maroon  = "#800000"
    * Red     = "#FF0000"
    * Purple  = "#800080"
    * Fuchsia = "#FF00FF"
    * Green   = "#008000" 
    * Lime    = "#00FF00" 
    * Olive   = "#808000" 
    * Yellow  = "#FFFF00" 
    * Navy    = "#000080" 
    * Blue    = "#0000FF" 
    * Teal    = "#008080" 
    * Aqua    = "#00FFFF" 
    """ #"

    role_text = {"package"           : "Package",
                 "module"            : "Module",
                 "class"             : "Class",
                 "method"            : "Method",
                 "function"          : "Function",
                 "module_attribute"  : "Module attribute",
                 "class_attribute"   : "Class attribute",
                 "instance_attribute": "Instance attribute",
                 "variable"          : "Name",
                 "parameter"         : "Argument",
                 "type"              : "Type",
                 "exception_class"   : "Exception class",
                 "exception"         : "Exception",
                 "warning_class"     : "Warning class",
                 "warning"           : "Warning"}
    """If an interpreted text has a role, we want to write that role
    out. We thus need a dictionary to relate role names to the text
    to be written out.
    """

    fancy = 0
    """Do we want fancy presentation?"""

    want_contents = 0
    """Do we *want* contents for this document?
    """

    language = "en"
    """The language that we believe our document to be
    destined for.
        """

    showwarnings = 1
    """Should we show warnings, or try to continue silently?
    """

    showinforms = 1
    """Should we show informational messages, or ignore them?
    """

    visible_targets = 0
    """Show link target names
    """

    visible_links = 0
    """Show link references (although not all of the possible links
    we produce).
    """

    def __init__(self):

        # The current document tree - unset it here just in case...
        self.document = None

        # Our HTML builder - ditto
        self.html = None

        # Our method cache - we seed it with the entry for "#text"
        # because we can't deduce the method name from that tag
        # ("write_#text" is not a valid Python name!)
        self.method_cache = {"#text":self.write_text}

    def __call__(self,document,stream):
        """Output an HTML representation of `document` to `stream`.

        Arguments:

            * document -- the DOCUTILS tree we are to output as HTML
            * stream   -- something like a File, with a write method
        """

        self.document = document
        self.html = buildhtml.BuildHTML(stream)

        # Reset things (i.e., so we can be called more than once)

        # The header level to use for <section> titles
        # (i.e., <h2>, <h3>, etc).
        self.level = 0

        # Have we output Contents for this document?
        self.got_contents = 0

        # Are we within the body of a field list item?
        self.infield = 0

        # Or in a paragraph? (note - only in the sense that the DOCUTILS
        # tree says that we're in a paragraph)
        self.in_paragraph = 0

        # Footnote autonumbers
        self.auto_footnote = 0
        """The current auto-numbered footnote's number.
        This will be stored as attribute "auto-index" on
        the footnote itself, by `find_auto_footnotes()`.
        """
        self.auto_footnote_names = {}
        """A dictionary linking an auto-numbered footnote label
        to the corresponding (generated) footnote number. This
        is populated by `find_auto_footnotes()`.
        """
        self.auto_footnote_list = []
        """A list of the auto-numbered footnote numbers that
        are used for non-named footnotes. This list is then
        used to populate the [#]_ footnote references. It is
        populated by `find_auto_footnotes()`.
        """
        self.auto_footnote_index = 0
        """An index into `self.auto_footnote_list`.
        """
        self.auto_footnote_target = 0
        """This is used to record the numbering for the "link" end
        of footnotes - i.e., the number for autonumbered references.
        """

        self.find_auto_footnotes(document)

        # Table location
        self.in_table_header = 0
        self.in_table_body = 0

        # And now down to work...
        self.html.write_doctype()
        self.html.start("html")

        # Hmm - have we been handed a "document" rooted tree,
        # or a DOM-like tree that has "document" as its single child?
        if document.tagname == "document":
            self.write_html(document,stream)
        else:
            for element in document:
                self.write_html(element,stream)

        self.html.end("html")
        self.html.finish()

    def find_auto_footnotes(self,element):
        """Locate and number autonumbered footnotes...
        """

        if element.tagname == "#text":
            return
        elif element.tagname == "footnote":
            # This is a footnote body - it is the footnote bodies
            # that determine their order and numbering...
            name = auto = None
            if element.hasattr("name"):
                name = element["name"]
            if element.hasattr("auto"):
                auto = element["auto"]
                self.auto_footnote += 1
                element["auto-index"] = self.auto_footnote
            if auto:
                if name:
                    if not self.auto_footnote_names.has_key(name):
                        self.auto_footnote_names[name] = self.auto_footnote
                    else:
                        # Well, what should we do?
                        # Removing it seems the best bet...
                        del self.auto_footnote_names[name]
                else:
                    self.auto_footnote_list.append(self.auto_footnote)

        for node in element:
            self.find_auto_footnotes(node)

    def write_document(self,element,stream):
        document_is_section = 0
        if element.hasattr("title"):
            title = self.html.escape(element["title"])
        else:
            firstchild = element[0]
            if firstchild.tagname == "title":
                title = self.html.escape(firstchild.astext())
                document_is_section = 1
            elif firstchild.hasattr("title"):
                title = self.html.escape(firstchild["title"])
            else:
                title = "Document produced by pysource"

        self.html.start("head")
        self.html.add("title",title)
        self.html.end("head")

        self.html.start("body")
        if document_is_section:
            # There is no internal <section> - instead we just
            # have a <document> whose first element is <title>
            # So, given that, pretend we ARE a section...
            self.write_section(element,stream)
        else:
            for node in element:
                self.write_html(node,stream)

        self.html.add("hr")
        self.html.start("p")
        self.html.add("em","Automatically generated by ",
                      self.html.element("code","pysource"),
                     " on %s\n"%time.ctime(time.time()))
        self.html.end("p")
        self.html.end("body")

    def write_text(self,element,stream):
        """Write out plain text.
        """
        self.html.add("text",self.html.escape(element.astext()))

    def write_html(self,element,stream):
        """Write out the HTML representation of `element` on `stream`.
        """

        name = element.tagname
        try:
            method = self.method_cache[name]
        except KeyError:
            method = getattr(self,"write_%s"%name,self.write_unknown)
            self.method_cache[name] = method
        method(element,stream)

    def write_unknown(self,element,stream):
        """Write out an element which we don't recognise.
        """

        self.html.add("p",self.html.element("comment","just a spacer"))

        self.html.start("font","&lt;%s"%element.tagname,color="red")
        for name,value in element.attlist():
            self.html.add("text"," %s='%s'"%(name,self.html.escape(value)))
        self.html.add("text","&gt;")
        self.html.end("font")

        for node in element:
            self.write_html(node,stream)

        self.html.add("font","&lt;/%s&gt;"%element.tagname,
                      color="red")

    def write_section(self,element,stream):
        """Write a section - i.e., something with a title
        """

        self.level += 1

        if element.hasattr("name"):
            # Lazily, escape the name so we don't have to worry
            # about single quotes in it...
            name=self.html.escape(element["name"])

            # Hmm - we *want* to write "\n\n<a name='...'></a>"
            # which isn't *quite* what this does - maybe have a specialised
            # call in buildhtml.py?
            self.html.add("text",self.html.element("a",name=name))
            if self.visible_links:
                self.html.start("font",color="green")
                self.html.add("text","&lt;%s&gt;"%name)
                self.html.end("font")

        for node in element:
            self.write_html(node,stream)

        self.level -= 1

    def write_title(self,element,stream):
        if 1 <= self.level <= 6:
            self.html.start("h%d"%self.level)
            for node in element:
                self.write_html(node,stream)
            self.html.end("h%d"%self.level)
        else:
            # Put a warning here?
            self.html.start("p")
            self.html.start("font",size="-1",color="red")
            self.html.add("text","[problem: header level='%d']"%self.level)
            self.html.end("font")
            self.html.start("strong")
            for node in element:
                self.write_html(node,stream)
            self.html.end("strong")
            self.html.end("p")

    def write_transition(self,element,stream):
        self.html.start("p", # hmm - strictly not legal...
                        self.html.element("hr"))

    def write_enumerated_list(self,element,stream):
        typedict = {"arabic" : "1",
                    "roman"  : "i",
                    "Roman"  : "I",
                    "alpha"  : "a",
                    "Alpha"  : "A"}
        try:
            enumtype = typedict[element["enumtype"]]
        except:
            enumtype = "1"

        # Does this match how DOCUTILS nodes work?
        if element.hasattr("start"):
            self.html.start("ol",type=enumtype,start=element["start"])
        else:
            self.html.start("ol",type=enumtype)
        for node in element:
            self.write_html(node,stream)
        self.html.end("ol")

    def write_bullet_list(self,element,stream):
        # Hmm - the translation is fairly arbitrary
        # - but at least consistent
        bulletdict = {"*" : "disc",
                      "-" : "circle",
                      "+" : "square"}
        try:
            bullet = bulletdict[element["bullet"]]
        except:
            bullet = None

        if bullet:
            self.html.start("ul",type=bullet)
        else:
            self.html.start("ul")
        for node in element:
            self.write_html(node,stream)
        self.html.end("ul")

    def write_definition_list(self,element,stream):
        self.html.start("dl")
        for node in element:
            self.write_html(node,stream)
        self.html.end("dl")

    def write_definition_list_item(self,element,stream):
        # Nothing special to do for this one
        for node in element:
            self.write_html(node,stream)

    def write_term(self,element,stream):
        self.html.start("dt")
        self.html.start("strong")
        for node in element:
            self.write_html(node,stream)
            self.html.add("text"," ") # to separate consecutive parts,
                                      # in option lists
        self.html.end("strong")
        self.html.end("dt")

    def write_list_item(self,element,stream):
        self.html.start("li")
        for node in element:
            self.write_html(node,stream)
        self.html.end("li")

    def write_option_list(self,element,stream):
        self.html.start("dl")
        for node in element:
            self.write_html(node,stream)
        self.html.end("dl")

    def write_option_list_item(self,element,stream):
        for node in element:
            self.write_html(node,stream)

    def write_option(self,element,stream):
        self.html.start("dt")
        self.html.start("strong")
        for node in element:
            self.write_html(node,stream)
            self.html.add("text"," ") # to separate consecutive parts,
                                      # in option lists
        self.html.end("strong")
        self.html.end("dt")

    def write_definition(self,element,stream):
        self.html.start("dd")
        for node in element:
            self.write_html(node,stream)
        self.html.end("dd")

    def write_short_option(self,element,stream):
        self.html.start("samp")
        for node in element:
            self.write_html(node,stream)
        self.html.end("samp")

    def write_long_option(self,element,stream):
        self.html.start("samp")
        for node in element:
            self.write_html(node,stream)
        self.html.end("samp")

    def write_vms_option(self,element,stream):
        self.html.start("samp")
        for node in element:
            self.write_html(node,stream)
        self.html.end("samp")

    def write_option_argument(self,element,stream):
        self.html.start("samp")
        for node in element:
            self.write_html(node,stream)
        self.html.end("samp")

    def write_description(self,element,stream):
        self.html.start("dd")
        for node in element:
            self.write_html(node,stream)
        self.html.end("dd")

    def write_field_list(self,element,stream):
        """Write out a fieldlist.
        """
        # The colour is for debugging purposes only!
        self.html.start("table",width="100%",bgcolor="palegreen")

        self.infield = 1
        for node in element:
            self.write_html(node,stream)
        self.infield = 0

        self.html.end("table")

    def write_field(self,element,stream):
        self.html.start("tr",valign="top",dps="field")
        for node in element:
            self.write_html(node,stream)
        self.html.end("tr")

    def write_field_name(self,element,stream):
        self.html.start("td",dps="field_name")
        self.html.start("strong")
        for node in element:
            self.write_html(node,stream)
        self.html.end("strong")
        self.html.end("td")

    def write_field_body(self,element,stream):
        self.infield = 1
        self.paranum = 0
        self.html.start("td",dps="field_body")
        for node in element:
            self.write_html(node,stream)
        self.html.end("td")
        self.infield = 0

    def write_biblio_field(self,element,stream,name):
        """Write out a document bibliographic datum.
        """
        self.infield = 1
        # The colour is for debugging purposes only!
        self.html.start("table",width="100%",bgcolor="palegreen")
        self.html.start("tr",valign="top")
        self.html.start("td",dps="biblio_field")
        self.html.add("strong",name)

        if len(element) != 1:
            raise HTMLError,"Found %d children in field %s"%\
                  (len(element),name)

        self.write_field_body(element[0],stream)

        self.html.end("td","tr","table")
        self.infield = 0

    def write_subtitle(self,element,stream):
        self.write_biblio_field(element,stream,"Subtitle")

    def write_author(self,element,stream):
        self.write_biblio_field(element,stream,"Author")

    def write_authors(self,element,stream):
        self.write_biblio_field(element,stream,"Authors")

    def write_organization(self,element,stream):
        self.write_biblio_field(element,stream,"Organisation")

    def write_organisation(self,element,stream):
        self.write_biblio_field(element,stream,"Organisation")

    def write_contact(self,element,stream):
        self.write_biblio_field(element,stream,"Contact")

    def write_version(self,element,stream):
        self.write_biblio_field(element,stream,"Version")

    def write_status(self,element,stream):
        self.write_biblio_field(element,stream,"Status")

    def write_date(self,element,stream):
        self.write_biblio_field(element,stream,"Date")

    def write_revision(self,element,stream):
        self.write_biblio_field(element,stream,"Revision")

    def write_copyright(self,element,stream):
        self.write_biblio_field(element,stream,"Copyright")

    def write_abstract(self,element,stream):
        self.write_biblio_field(element,stream,"Abstract")

    def write_reference(self,element,stream):
        """Write a link - the "pointer" to a target.

        Doesn't yet handle "indirect" links...
        """
        if element.hasattr("refuri"):
            name = self.html.escape(element["refuri"])
            self.html.start("a",href=name)
        elif element.hasattr("refname"):
            # Escape the name to match what we do with titles...
            name = "#"+self.html.escape(element["refname"])
            self.html.start("a",href=name)
        else:
            self.write_unknown(element,stream)
            return
        for node in element:
            self.write_html(node,stream)
        self.html.end("a")
        if self.visible_links:
            self.html.start("font",color="green")
            self.html.add("text","&lt;%s&gt;"%name)
            self.html.end("font")

    def write_target(self,element,stream):
        """Write the target end of a link.

        Ultimately, the user should be able to choose to fold these into
        the document (i.e., into the "link" itself).
        """
        try:
            name = element["name"]
        except:
            name = "**no target name**"

        ##if not self.in_paragraph:
        ##    self.html.start("p")

        # Provide some "debugging" information
        if self.visible_targets:
            self.html.start("font",color="green")
            self.html.start("strong")
            self.html.start("em")
            self.html.add("text",name)
            self.html.end("em")
            self.html.end("strong")
            self.html.end("font")

        self.html.add("a",name=self.html.escape(name))

        if element.has_key("refuri"):
            uri = self.html.escape(element["refuri"])
            self.html.add("text",":&nbsp;")
            self.html.add("a",element["refuri"],href=uri)

        ##if not self.in_paragraph:
        ##    self.html.end("p")

    def write_footnote(self,element,stream):
        """Write out the body of a footnote.

        It's not entirely clear how to do this in HTML, so we'll
        just try for something distinctive...
        """
        name = auto = index = None
        if element.hasattr("name"):
            name = element["name"]
        if element.hasattr("auto"):
            auto = element["auto"]
        if element.hasattr("auto-index"):
            index = element["auto-index"]

        if auto and index == None:
            raise HTMLError,"Footnote auto-numbering has not been done"

        if name and auto:
            self.html.start("p")
            self.html.add("a",name=self.html.escape(name))
            self.html.add("text","   ")
            self.html.add("a",name=`index`)
            self.html.end("p")
        elif name:
            self.html.start("p")
            self.html.add("a",name=self.html.escape(name))
            self.html.end("p")
        elif auto:
            self.html.start("p")
            self.html.add("a",name=`index`)
            self.html.end("p")
        else:
            self.write_message(stream,level=2,
                               text="Footnote doesn't have name"
                               " or auto attributes")

        self.html.start("table",align="center",width="80%")
        self.html.start("tr")
        self.html.start("td")

        # Automatically numbered footnotes don't contain an explicit
        # <label> element, so we have to do it by hand...
        if auto:
            self.write_label(element,stream,index)

        for node in element:
            self.write_html(node,stream)

        self.html.end("td","tr","table")

    def write_label(self,element,stream,index=None):
        """Write out the label for a footnote.
        """
        self.html.add("p",
                      self.html.element("hr"))

        self.html.start("p","Footnote ")
        if index == None:
            self.html.add("strong",element.astext())
        else:
            self.html.add("strong",`index`)
        self.html.end("p")
        self.html.add("br")
        self.html.add("hr")

    def write_footnote_reference(self,element,stream):
        """Write out the link to a footnote.
        """
        name = auto = None
        if element.hasattr("refname"):
            name = element["refname"]
        if element.hasattr("auto"):
            auto = element["auto"]

        if auto:
            if name:
                if self.auto_footnote_names.has_key(name):
                    number = self.auto_footnote_names[name]
                else:
                    self.write_message(stream,level=2,
                                       text="Autonumber footnote name"
                                       " '%s' doesn't match a"
                                       " footnote"%name)
                    number = 0
            else:
                try:
                    number = self.auto_footnote_list[self.auto_footnote_index]
                    self.auto_footnote_index += 1
                except IndexError:
                    # Hmm - probably a [#]_ with no footnotes for it to
                    # point to - best we can do is write *something* out
                    self.html.start("font",color=self.colours["Error"])
                    self.html.add("text","[")
                    self.html.add("strong","#")
                    self.html.add("text","]")
                    self.html.end("font")
                    return

        if auto:
            self.html.start("a",href="#%d"%number)
            self.html.add("text","[")
            self.html.add("strong",`number`)
            self.html.add("text","]")
            self.html.end("a")
        elif name:
            self.html.start("a",href="#%s"%self.html.escape(name))
            self.html.add("text","[")
            self.html.add("strong",element.astext())
            self.html.add("text","]")
            self.html.end("a")
        else:
            self.write_unknown(element,stream)
            return


    def write_comment(self,element,stream):
        """Write out a comment
        """
        # Technically, this may not be enough, as we don't know what
        # they might *have* in the comment - but in practice, it's
        # likely to do...
        self.html.start("comment")
        for node in element:
            self.write_html(node,stream)
        self.html.end("comment")

    def write_paragraph(self,element,stream):
        """Write a new paragraph.

        This is a method simply because I do odd things inside
        a field body, to make it "look" better.
        """
        if self.html.last_tag() not in ["li","dd","td","th"]:
            self.html.start("p")
            write_slash_p = 1
        else:
            write_slash_p = 0

        for node in element:
            self.write_html(node,stream)

        if write_slash_p:
            self.html.end("p")
        return

        
        self.in_paragraph = 1
        if self.infield:
            self.write_field_paragraph(element,stream)
        else:
            self.html.start("p")
            for node in element:
                self.write_html(node,stream)
            self.html.end("p")
        self.in_paragraph = 0

    def write_field_paragraph(self,element,stream):
        """Write a new paragraph inside a field body.
        """
        started_row = 0
        if self.paranum > 0:
            self.html.start("tr")
            self.html.add("td")   # an empty column...
            self.html.start("td",dps="paragraph")
            started_row = 1
        self.paranum += 1
        for node in element:
            self.write_html(node,stream)
        if self.paranum > 1:
            self.html.end("td")
        if started_row:
            self.html.end("tr")

    def write_table(self,element,stream):
        """Write out a table - initially in a very visible manner
        """
        self.html.start("table",border="1",align="center")
        for node in element:
            self.write_html(node,stream)
        self.html.end("table")

    def write_tgroup(self,element,stream):
        for node in element:
            self.write_html(node,stream)

    def write_colspec(self,element,stream):
        for node in element:
            self.write_html(node,stream)

    def write_rowspec(self,element,stream):
        for node in element:
            self.write_html(node,stream)

    def write_thead(self,element,stream):
        """Write out a table header section
        """
        self.html.start("thead")
        self.in_table_header = 1
        for node in element:
            self.write_html(node,stream)
        self.in_table_header = 0
        self.html.end("thead")

    def write_tbody(self,element,stream):
        """Write out a table body section
        """
        self.html.start("tbody")
        self.in_table_body = 1
        for node in element:
            self.write_html(node,stream)
        self.in_table_body = 0
        self.html.end("tbody")

    def write_row(self,element,stream):
        """Write out a table row.
        """
        if self.in_table_header:
            self.html.start("tr",valign="top",align="left")
        else:
            self.html.start("tr",valign="top")
        for node in element:
            self.write_html(node,stream)
        self.html.end("tr")

    def write_entry(self,element,stream):
        """Write out the equivalent of a table "entry" (e.g., HTML <td>)
        """
        keywords = {}
        if element.hasattr("morecols"):
            keywords["colspan"] = element["morecols"]+1
        if element.hasattr("morerows"):
            keywords["rowspan"] = element["morerows"]+1
        keywords["dps"] = "entry"
        self.html.start("td",**keywords)

        if self.in_table_header:
            self.html.start("strong")

        if len(element) == 0:
            # Since we're writing a table with a border, it looks better if
            # even an empty entry has *some* content - a non-breaking space
            # will suffice to make sure the entry is properly bordered
            self.html.add("text","&nbsp;")
        else:
            for node in element:
                self.write_html(node,stream)

        if self.in_table_header:
            self.html.end("strong")

        self.html.end("td")

    def write_message(self,stream,level=2,text=None,element=None):
        names = {1: "Information",
                 2: "Warning",
                 3: "Error",
                 4: "Fatal"}

        if level == 0 and not self.showinforms:
            return
        elif level == 1 and not self.showwarnings:
            return

        try:
            name = names[level]
            colour = self.colours[name]
        except:
            name = "Unrecognised warning level %d"%level
            colour = self.colours["Fatal"]
        bgcolour = self.colours["WarningBG"]

        self.html.start("table",width="100%%",bgcolor=bgcolour)
        self.html.start("tr")
        self.html.start("td")
        self.html.start("font",color=colour)
        self.html.add("strong",name)
        self.html.end("font")
        self.html.end("td","tr")

        self.html.start("tr")
        self.html.start("td")
        if text:
            self.html.add("text",text)
        if element:
            for node in element:
                self.write_html(node,stream)
        self.html.end("td","tr")
        self.html.end("table")

    def write_problematic(self,element,stream):
        self.html.start("a",href="#%s"%element["refid"])
        self.html.start("font",color="red")
        for node in element:
            self.write_html(node,stream)
        self.html.end("font")
        self.html.end("a")

    def write_system_message(self,element,stream):
        try:
            target = element["refid"]
        except:
            target = None
        if target:
            self.html.add("a","",name="%s"%target)
        self.write_message(stream,level=element["level"],element=element)

    def write_group(self,element,stream):
        """By default, we don't do anything with <group> tags.
        """
        for node in element:
            self.write_html(node,stream)

    def write_emphasis(self,element,stream):
        self.html.start("em")
        for node in element:
            self.write_html(node,stream)
        self.html.end("em")

    def write_strong(self,element,stream):
        self.html.start("strong")
        for node in element:
            self.write_html(node,stream)
        self.html.end("strong")

    def write_interpreted(self,element,stream):
        if element.hasattr("refname"):
            self.html.start("a",href="#"+self.html.escape(element["refname"]))
            inref = 1
        else:
            inref = 0

        self.html.start("samp",style="interpreted")
        if element.hasattr("role"):
            role = element["role"]
            if self.role_text.has_key(role):
                self.html.add("text","%s "%self.role_text[role])

        for node in element:
            self.write_html(node,stream)
        self.html.end("samp")

        if inref:
            self.html.end("a")
            if self.visible_links:
                self.html.start("font",color="green")
                self.html.add("text","&lt;%s&gt;"%element["refname"])
                self.html.end("font")

    def write_literal(self,element,stream):
        self.html.start("samp",style="literal")
        for node in element:
            self.write_html(node,stream)
        self.html.end("samp")

    def write_literal_block(self,element,stream):
        self.html.start("pre")
        for node in element:
            self.write_html(node,stream)
        self.html.end("pre")

    def write_doctest_block(self,element,stream):
        self.html.start("pre",style="doctest")
        for node in element:
            self.write_html(node,stream)
        self.html.end("pre")

    def write_block_quote(self,element,stream):
        self.html.start("blockquote")
        for node in element:
            self.write_html(node,stream)
        self.html.end("blockquote")


# ----------------------------------------------------------------------
class PythonWriter(Writer):
    """A Writer that understands how Python is represented using DOCUTILS.

    Note that as a princple, I don't believe that "modes" should add extra
    DOCUTILS nodes - in other words, any DOCUTILS tree should be presentable by
    the default writer (albeit maybe not very well).
    """

    python_colours = {"package"   : "Aqua",
                      "module"    : "#FFFF00",
                      "class"     : "#99CCFF",  # Python blue
                      "method"    : "#AAFFAA",
                      "function"  : "#FFAAFF",
                      "docstring" : "#FFFFCC",  # pale yellow-ish
                      "generator" : "pink",
                      }

    def write_group(self,element,stream):
        """Write out a group according to its style.
        """

        try:
            style = element["style"]
        except:
            style = None

        if style == "docstring":
            self.write_docstring(element,stream)
        elif style == "generator":
            self.html.start("table",bgcolor=self.python_colours["generator"],
                            width="100%")
            self.html.start("tr")
            self.html.start("td")

            for node in element:
                self.write_html(node,stream)

            self.html.end("td")
            self.html.end("tr")
            self.html.end("table")
        else:
            for node in element:
                self.write_html(node,stream)

    def write_docstring(self,element,stream):
        """Write out a docstring
        """

        self.html.start("table",bgcolor=self.python_colours["docstring"],
                        width="100%")
        self.html.start("tr")
        self.html.start("td")

        # Within a docstring, we want traditional HTML headers,
        # starting at <h2> and working downwards
        self.level = 1

        for node in element:
            self.write_html(node,stream)

        self.html.end("td")
        self.html.end("tr")
        self.html.end("table")

    def write_section(self,element,stream):
        """Write a section - i.e., something with a title
        """

        try:
            style = element["style"]
        except:
            style = None

        if style in ["package","module","class","method","function"]:
            self.write_py_section(element,stream,style)
        else:
            Writer.write_section(self,element,stream)

    def write_py_section(self,element,stream,style):
        """Write out the HTML for a Python section.
        """

        self.level += 1

        try:
            colour = self.python_colours[style]
        except:
            colour = self.colours["default"]

        self.html.start("table",width="100%",cellspacing="0")

        # First row - full width, coloured
        self.html.start("tr",bgcolor=colour)
        self.html.add("td",self.html.element("hr"),colspan="2")
        self.html.end("tr")

        # Now, if the section has a <title> and a <group> with
        # style="details", we want to treat them specially - and
        # we *know* that they should, if present, be the first
        # two child elements...
        # Optional first-and-a-halfth row
        offset = self.write_py_section_details(element,stream,colour)

        if len(element.children) > offset:
            # Second row - left hand margin coloured, "body" not
            self.html.start("tr")
            self.html.add("td","&nbsp;",width="1%",bgcolor=colour)
            self.html.start("td",width="99%")

            # More detail needed here?
            for node in element.children[offset:]:
                self.write_html(node,stream)

            self.html.end("td")
            self.html.end("tr")

        # Third row - full width coloured
        self.html.start("tr",bgcolor=colour)
        self.html.add("td",self.html.element("hr"),colspan="2")
        self.html.end("tr")

        if self.got_contents:
            # Fourth row - full width coloured
            self.html.start("tr",bgcolor=colour)
            self.html.add("td","&nbsp;")
            self.html.start("td",align="right")
            self.html.add("text","Return to ")
            self.html.add("a","Contents",href="#contents")
            self.html.end("td")
            self.html.end("tr")

        self.html.end("table")

        self.level -= 1

    def write_py_section_details(self,element,stream,colour):
        """Deal with the title and details for a Python section.

        Returns the offset within the <section> elements children
        at which we should continue processing...
        """
        offset = 0
        if element[0].tagname == "title":
            offset = 1
            self.html.start("tr",bgcolor=colour)
            self.html.start("td",colspan="2")
            self.html.start("table",width="100%")
            self.html.start("tr",valign="top")
            self.html.start("td")
            # Hmm. It doesn't work too well using <h2>..<h6> as header
            # elements for nested module, class, etc. Let's do this
            # by "hand"
            self.html.start("font",size="+2")
            for node in element[0]:
                self.write_html(node,stream)
            self.html.end("font")
            self.html.end("td")
            self.html.end("tr")

            if element[1].tagname == "group" and \
               element[1].hasattr("style") and \
               element[1]["style"] == "details":
                offset = 2
                self.html.start("tr")
                self.html.start("td",align="right")
                self.write_html(element[1],stream)
                self.html.end("td")
                self.html.end("tr")
            self.html.end("table")
            self.html.end("td")
            self.html.end("tr")
        return offset
