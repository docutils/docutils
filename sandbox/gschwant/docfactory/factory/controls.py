"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.3
"""

import os, throbimages, time
from   wxPython.wx           import *
from   wxPython.stc          import *
from   docutilsadapter       import get_rest_bibl_fields
from   docutilsadapter       import get_rest_directives
from   dialogs               import customMsgBox
from   shutil                import copyfile

if wxPlatform == '__WXMSW__':
    faces = { 'times': 'Times New Roman',
              'mono' : 'Courier New',
              'helv' : 'Arial',
              'other': 'Comic Sans MS',
              'size' : 10,
              'size2': 8,
             }
else:
    faces = { 'times': 'Times',
              'mono' : 'Courier',
              'helv' : 'Helvetica',
              'other': 'new century schoolbook',
              'size' : 12,
              'size2': 10,
             }

#---------------------------------------------------------------------------
    
class CustomStyledTextCtrl(wxStyledTextCtrl):
    def __init__(self, parent, ID, log):
        wxStyledTextCtrl.__init__(self, parent, ID)
        self.log = log
        
        self.CmdKeyAssign(ord('L'), wxSTC_SCMOD_CTRL, wxSTC_CMD_ZOOMIN)
        self.CmdKeyAssign(ord('K'), wxSTC_SCMOD_CTRL, wxSTC_CMD_ZOOMOUT)
        self.SetEdgeMode(wxSTC_EDGE_LINE)
        self.SetEdgeColour(wxColour(175,255,255))
        self.SetEdgeColumn(75)
        self.SetUseTabs(0)
        self.SetIndent(2)
        self.SetEOLMode(wxSTC_EOL_LF)

        self.set_styles()

        EVT_STC_MODIFIED(self, ID, self.OnModified)
        EVT_CHAR(self, self.OnChar)
        
        # lists for autocompletion:
        self.bibliographic_fields = get_rest_bibl_fields()
        self.directives = get_rest_directives()

        self.IsModified = 0
        
    def set_styles(self):
        
        # general
        self.StyleSetSpec(wxSTC_STYLE_DEFAULT, 'face:%(mono)s,size:%(size)d' % faces)
        self.StyleSetSpec(1, 'face:%(mono)s,bold,fore:#0000FF,size:%(size)d' % faces)
        self.StyleSetSpec(2, 'face:%(mono)s,italic,fore:#FF0000,size:%(size)d' % faces)
        self.StyleSetSpec(3, 'face:%(mono)s,bold,size:%(size)d' % faces)
        self.StyleSetSpec(4, 'face:%(mono)s,size:%(size)d' % faces)
        self.StyleSetSpec(wxSTC_STYLE_CONTROLCHAR, 'face:%(other)s' % faces)

        # line numbers in the margin
        self.StyleSetSpec(wxSTC_STYLE_LINENUMBER,
                          'back:#C0C0C0,face:%(helv)s,size:%(size2)d' % faces)
        self.SetMarginType(0, wxSTC_MARGIN_NUMBER)
        self.SetMarginWidth(0, 3*faces['size']-2)

        # markers
        self.SetMarginType(1, wxSTC_MARGIN_SYMBOL)
        self.MarkerDefine(0, wxSTC_MARK_CIRCLE, 'RED', '#FFFF00')
        self.MarkerDefine(1, wxSTC_MARK_CIRCLE, '#FFFF00', 'RED')
        
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
        self.IsModified = 1

    def set_lexer(self, ext):
        lexer = self.GetLexer()
        if ext == '.html' or ext == '.htm':
            if lexer != wxSTC_LEX_HTML:
                self.SetLexer(wxSTC_LEX_HTML)
        else:
            if lexer != wxSTC_LEX_NULL:
                self.SetLexer(wxSTC_LEX_NULL)

    def LoadFile(self, filename):
        try:
            wxBeginBusyCursor()
            try:
                f = open(filename, 'rt')
                text = f.read()
                f.close()
                ext = os.path.splitext(filename)[1]
                self.set_lexer(ext)
                self.SetText(text)
                self.EmptyUndoBuffer()
                self.IsModified = 0
            except:
                self.Clear()
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        finally:
            wxEndBusyCursor()
            
    def SaveFile(self, filename, backup=0):
        return_value = 1
        try:
            wxBeginBusyCursor()
            try:
                if backup:
                    copyfile(filename, '%s.bak' % filename)
                f = open(filename, 'w')
                f.write(self.GetText())
                f.close()
                self.IsModified = 0
            except:
                return_value = 0
                customMsgBox(self, 'Error when saving "%s".\n\n%s:\n%s\n%s' \
                             % (filename, sys.exc_info()[0], sys.exc_info()[1],
                                sys.exc_info()[2]),
                             'error')
        finally:
            wxEndBusyCursor()
        return return_value

    def Clear(self):
        self.SetText('')
        self.IsModified = 0

#---------------------------------------------------------------------------

class CustomStatusBar(wxStatusBar):
    def __init__(self, parent):
        wxStatusBar.__init__(self, parent, -1)
        self.SetFieldsCount(2)
        self.SetStatusWidths([-1,150])

        self.SetStatusText('Ready', 0)

        # start timer
        self.timer = wxPyTimer(self.Notify)
        self.timer.Start(1000)
        self.Notify()

    # Time-out handler
    def Notify(self):
        t = time.localtime(time.time())
        st = time.strftime("  %d-%b-%Y   %H:%M:%S", t)
        self.SetStatusText(st, 1)

#---------------------------------------------------------------------------

class CustomTreeCtrl(wxTreeCtrl):
        
    def OnCompareItems(self, item1, item2):
        t1 = self.GetItemText(item1)
        t2 = self.GetItemText(item2)
        #self.log.WriteText('compare: ' + t1 + ' <> ' + t2 + '\n')
        if t1 < t2: return -1
        if t1 == t2: return 0
        return 1

