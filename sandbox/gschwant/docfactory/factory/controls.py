"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.3
"""

from wxPython.wx     import *
from wxPython.stc    import *
from docutilsadapter import get_rest_bibl_fields
from docutilsadapter import get_rest_directives
from dialogs         import customMsgBox

if wxPlatform == '__WXMSW__':
    face1 = 'Arial'
    face2 = 'Times New Roman'
    face3 = 'Courier New'
    pb = 10
else:
    face1 = 'Helvetica'
    face2 = 'Times'
    face3 = 'Courier'
    pb = 10
    
class ReSTstc(wxStyledTextCtrl):
    def __init__(self, parent, ID, log):
        wxStyledTextCtrl.__init__(self, parent, ID)
        self.log = log
        self.IsModified = false
        
        self.CmdKeyAssign(ord('L'), wxSTC_SCMOD_CTRL, wxSTC_CMD_ZOOMIN)
        self.CmdKeyAssign(ord('K'), wxSTC_SCMOD_CTRL, wxSTC_CMD_ZOOMOUT)
        self.SetEdgeMode(wxSTC_EDGE_BACKGROUND)
        self.SetEdgeColour(wxColour(175,255,255))
        self.SetEdgeColumn(77)
        self.SetUseTabs(false)
        self.SetIndent(2)
        #self.SetViewEOL(true)
        self.SetEOLMode(wxSTC_EOL_LF)

        # make some styles
        self.StyleSetSpec(wxSTC_STYLE_DEFAULT, 'size:%d,face:%s' % (pb, face3))
        #self.StyleSetSpec(1, "size:%d,bold,face:%s,fore:#0000FF" % (pb+2, face1))
        #self.StyleSetSpec(2, "face:%s,italic,fore:#FF0000,size:%d" % (face2, pb))
        #self.StyleSetSpec(3, "face:%s,bold,size:%d" % (face2, pb+2))
        #self.StyleSetSpec(4, "face:%s,size:%d" % (face1, pb-1))

        # line numbers in the margin
        self.SetMarginType(0, wxSTC_MARGIN_NUMBER)
        self.SetMarginWidth(0, 3*pb-2)
        self.StyleSetSpec(wxSTC_STYLE_LINENUMBER, "size:%d,face:%s" % (pb-2, face1))

        # setup some markers
        self.SetMarginType(1, wxSTC_MARGIN_SYMBOL)
        self.MarkerDefine(0, wxSTC_MARK_CIRCLE, "RED", "#FFFF00")
        self.MarkerDefine(1, wxSTC_MARK_CIRCLE, "#FFFF00", "RED")
        
        EVT_STC_MODIFIED(self, ID, self.OnModified)
        EVT_CHAR(self, self.OnChar)

        # lists for autocompletion:
        self.bibliographic_fields = get_rest_bibl_fields()
        self.directives = get_rest_directives()

    def OnChar(self, event):
        key = event.KeyCode()
        currpos = self.GetCurrentPos()
        currcol = self.GetColumn(currpos)
        if currcol == 0 and key == ord(':'):
            self.AddText(':')
            self.AutoCompShow(0, self.bibliographic_fields)
        elif currcol == 2 and key == ord(' '):
            if self.GetCharAt(currpos - 1) == self.GetCharAt(currpos - 2) == ord('.'):
                self.AddText(' ')
                self.AutoCompShow(0, self.directives)
            else:
                event.Skip()
        else:
            event.Skip()

    def OnModified(self, evt):
        self.IsModified = true

    def LoadFile(self, filename):
        try:
            wxBeginBusyCursor()
            try:
                f = open(filename, 'rt')
                text = f.read()
                f.close()
                self.SetText(text)
                self.EmptyUndoBuffer()
                self.IsModified = false
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        finally:
            wxEndBusyCursor()
            
    def SaveFile(self, filename):
        try:
            wxBeginBusyCursor()
            try:
                #self.ConvertEOLs(wxSTC_EOL_LF)
                f = open(filename, 'w')
                f.write(self.GetText())
                f.close()
                self.IsModified = false
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        finally:
            wxEndBusyCursor()

    def Clear(self):
        self.SetText('')
        self.IsModified = false
