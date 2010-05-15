# Author: mauriciocap <mauriciocap@gmail.com>
# Copyright: inherited from docutils.sf.net
# MCAP

"""
Directive to generate uml diagrams using plantuml

Options:
  - path: prefix without extension (e.g. "images/login" will generate images/login.txt and images/login.png" ). Files will usualy be OVERWRITEN. But: if no content is specified and .txt file exists, png will be generated from this txt
"""

__docformat__ = 'reStructuredText'

from docutils.parsers.rst.directives.images import Image
from docutils.parsers.rst import directives
import os

class Uml(Image):
    required_arguments = 0
    optional_arguments = 1
    option_spec = Image.option_spec.copy()
    has_content = True

    def run(self):
        fname= "uml/%06d" % self.lineno #A: default
        if (len(self.arguments)>0):
            fname= self.arguments[0]
        #A: path contains the path for the txt and image without extension
        (fnameOutDir, fnameBase)= os.path.split(fname)
        txtFname= fname+".txt"
        imgFname= fname+".png"
        if self.content:
            os.path.isdir(fnameOutDir) or os.mkdir(fnameOutDir)
            #A: fnameOutDir exists, BUT only ONE level will be created
            fo= open(txtFname,"wb")
            fo.write("@startuml\n")
            try:
                fo.write(self.state_machine.document.settings.plantuml_hdr+"\n")
            except AttributeError:
                pass

            fo.write('\n'.join(self.content))

            fo.write("\n@enduml\n")
            fo.close()
            #A: txt file OVERWRITEN!
        if (not self.state.document.settings.file_insertion_enabled):
            warning = self.state_machine.reporter.warning(
              'File and URL access deactivated; ignoring "%s" '
              'directive.' % self.name, nodes.literal_block(
                    self.block_text, self.block_text), line=self.lineno)
            return [warning]

        plantumlCmd= "plantuml %s"
        try:
            plantumlCmd= self.state_machine.document.settings.plantuml_cmd
        except AttributeError:
            pass
        os.system(plantumlCmd % txtFname)
        self.arguments= [imgFname]
        return Image.run(self)
