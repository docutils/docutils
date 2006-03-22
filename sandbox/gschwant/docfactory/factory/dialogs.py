"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.3
"""

from   wxPython.lib.throbber         import Throbber
from   wxPython.wx                   import *
from   wxPython.help                 import *
from   wxPython.lib.filebrowsebutton import DirBrowseButton
from   docutils.utils                import relative_path
from   urllib                        import quote
from   docutilsadapter               import language_codes, publishers
import images, os, string, ConfigParser, stylesheets, throbimages, wx

NAME = 'DocFactory'

#---------------------------------------------------------------------------

def customMsgBox(window, text, type=None):
   if type=='info':
      style=wxOK|wxICON_INFORMATION
   elif type=='error':
      style=wxOK|wxICON_ERROR
   elif type=='wakeup':
      style=wxOK|wxICON_EXCLAMATION
   else:
      style=wxOK
   dlg = wxMessageDialog(window, text, NAME, style)
   dlg.Centre()
   dlg.ShowModal()
   dlg.Destroy()

#---------------------------------------------------------------------------

class aboutDlg(wxDialog):

    def __init__(self, parent):
        wxDialog.__init__(self, parent, -1, title='About')
        self.SetBackgroundColour(wxColour(0, 0, 0))
        self.SetForegroundColour(wxColour(0, 255, 0))
        self.SetFont(wxFont(10, wxMODERN, wxNORMAL, wxNORMAL, false))

        bmp = images.getLogoBigBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        wxStaticBitmap(self, -1, bmp, wxPoint(42, 8))
        wxStaticBitmap(self, -1, bmp, wxPoint(274, 8))

        # throbber
        pics = [throbimages.catalog[i].getBitmap()
                for i in throbimages.index
               ]

        Throbber(self, -1, pics, pos=wxPoint(74, 11),
                 size=(200, 25), frameDelay = 0.065, reverse=true).Start()        

        #text = wxStaticText(self, -1, 'DocFactory 0.2', wxPoint(50, 8))
        #text.SetFont(wxFont(20, wxSWISS, wxNORMAL, wxBOLD, false))

        text = wxStaticText(self , -1,
                            '>>> release:           0.3',
                            wxPoint(9, 50))
        text = wxStaticText(self, -1,
                            '>>> manufactured by:   gunnar schwant',
                            wxPoint(9, 65))
        text = wxStaticText(self, -1,
                            '>>> mailto:            g.schwant@gmx.de',
                            wxPoint(9, 80))
        text = wxStaticText(self, -1,
                            '>>> Python version:    2.3.2 (or later)',
                            wxPoint(9, 95))
        text = wxStaticText(self, -1,
                            '>>> wxPython version:  2.4.2.4',
                            wxPoint(9, 110))
        text = wxStaticText(self, -1,
                            '>>> Docutils version:  0.3 (or later)',
                            wxPoint(9, 125))
        text = wxStaticText(self, -1,
                            '>>> special thanks to: guido van rossum,',
                            wxPoint(9, 140))
        text = wxStaticText(self, -1,
                            '...                    robin dunn',
                            wxPoint(9, 155))
        text = wxStaticText(self, -1,
                            '...                    and david goodger',
                            wxPoint(9, 170))
        text = wxStaticText(self, -1,
                            '>>> visit http://docutils.sourceforge.net',
                            wxPoint(9, 185))
        self.Fit()

#---------------------------------------------------------------------------

class projectSettingsDlg(wxDialog):

    def __init__(self, parent, project, invalid_names):
        wxDialog.__init__(self, parent, -1, title = 'Project Settings')

        self.project = project
        self.configfile = os.path.join(project.directory, 'docutils.conf')
        self.invalid_names = invalid_names

        # Help Button
        wxContextHelpButton(self, pos = wxPoint(370, 8),
                            size = wxSize(23, 23))

        # Output Directory
        wxStaticText(self, -1, 'Output-Directory', wxPoint(8, 42))
        exitID = wxNewId()
        self.dirCtrl    = wxTextCtrl(self, exitID,
                                     pos = wxPoint(96, 40),
                                     size = wxSize(240, 21))
        self.dirCtrl.SetHelpText('This is the default ' + \
                                 'directory for output-files.')
        exitID = wxNewId()
        self.btnSelDir  = wxButton(self, exitID, 'Select',
                                   pos = wxPoint(344, 40),
                                   size = wxSize(75, 23))
        EVT_BUTTON(self, exitID, self.onBtnSelDir)

        # Title
        wxStaticText(self, -1, 'Title', wxPoint(8, 74))
        exitID = wxNewId()
        self.nameCtrl = wxTextCtrl(self, exitID,
                                   pos = wxPoint(96, 72),
                                   size = wxSize(240, 21),
                                   validator = customValidator(PROJECT_NAME))
        self.nameCtrl.SetHelpText('Enter a title for your project here.')


        #--------------------------------------------------
        # Fill controls with initial values
        self.init_ctrls()
        
        #--------------------------------------------------
        # docutils.conf-, OK- and Cancel-Buttons
        du_position = wxPoint(50,110)
        ok_position = wxPoint(260,110)
        cancel_position = wxPoint(344,110)
        exitID = wxNewId()
        self.du         = wxButton(self, exitID, 'Edit docutils.conf',
                                   pos = du_position,
                                   size = wxSize(-1, 23))
        EVT_BUTTON(self, exitID, self.onBtnDocutils)
        exitID = wxNewId()
        self.ok         = wxButton(self, exitID, 'OK',
                                   pos = ok_position,
                                   size = wxSize(75, 23))
        EVT_BUTTON(self, exitID, self.onBtnOk)
        self.cancel     = wxButton(self, wxID_CANCEL, 'Cancel',
                                   pos = cancel_position,
                                   size = wxSize(75, 23))
        self.ok.SetDefault()

        #--------------------------------------------------
        # Dialog Sizing
        self.Fit()

    def onBtnSelDir(self, event):
        dir = self.dirCtrl.GetValue()
        if not os.path.isdir(dir):
            dir = ''
        dlg = wxDirDialog(self, 'Choose a directory for output files', dir, style=wx.DD_DEFAULT_STYLE|wx.DD_NEW_DIR_BUTTON)
        if dlg.ShowModal() == wxID_OK:
            directory = dlg.GetPath()
            self.project.directory = directory
            newconfig = os.path.join(directory, 'docutils.conf')
            if newconfig != self.configfile:
                self.configfile = newconfig
                self.init_ctrls()
        dlg.Destroy()
      
    def getValues(self):
        return(self.nameCtrl.GetValue(), self.dirCtrl.GetValue())

    def onBtnOk(self, event):
        dir = self.dirCtrl.GetValue()
        name = self.nameCtrl.GetValue()
        if not os.path.isdir(dir):
            customMsgBox(self, 'Invalid Output-Directory.', 'wakeup')
        elif name == '':
            customMsgBox(self, 'You have to enter a Project-Title.', 'wakeup')
        elif (name in self.invalid_names):
            customMsgBox(self,
                         'Invalid Project-Title. Please try another title.',
                         'wakeup')
        else:
            self.EndModal(event.GetId())

    def onBtnDocutils(self, event):
        if os.path.isdir(self.dirCtrl.GetValue()):
            dlg = configEditDlg(self, self.configfile)
            dlg.ShowModal()
        else:    
            customMsgBox(self, 'Invalid Output-Directory.', 'wakeup')

    def init_ctrls(self):
        self.nameCtrl.SetValue(self.project.name)
        self.dirCtrl.SetValue(self.project.directory)

#---------------------------------------------------------------------------

class hyperlinkDlg(wxDialog):

    def __init__(self, parent, directory, project=None):
        wxDialog.__init__(self, parent, -1, title = 'Hyperlink')
        btn_size = wxSize(75, 23)
        self.directory = directory
        self.project = project
        wxStaticText(self, -1, 'Path?', wxPoint(8, 12))
        exitID = wxNewId()
        self.pathCtrl = wxTextCtrl(self, exitID,
                                   pos = wxPoint(46, 10),
                                   size = wxSize(240, 21))
        exitID = wxNewId()
        self.btnSelPath = wxButton(self, exitID, 'Select',
                                   pos = wxPoint(294, 10),
                                   size = btn_size)
        EVT_BUTTON(self, exitID, self.onBtnSelPath)
        self.ok         = wxButton(self, wxID_OK, 'OK',
                                   pos = wxPoint(185-80,40),
                                   size = btn_size)
        self.cancel     = wxButton(self, wxID_CANCEL, 'Cancel',
                                   pos = wxPoint(185+5,40),
                                   size = btn_size)
        self.ok.SetDefault()
        self.Fit()

    def onBtnSelPath(self, event):
        go_ahead = 1
        dlg = wxFileDialog(self, 'Select a file',
                           self.directory, '', '*.*',
                           wxOPEN|wxFILE_MUST_EXIST)
        if dlg.ShowModal() == wxID_OK:
            path = dlg.GetPath()
        else:
            go_ahead = 0
        dlg.Destroy()
        if go_ahead:
            if self.project == None:
                dlg = wxDirDialog(self, 'Calculate path relative'
                                  ' to which outputdirectory?',
                                  self.directory)
                if dlg.ShowModal() == wxID_OK:
                    self.directory = dlg.GetPath()
                else:
                    go_ahead = 0
                dlg.Destroy()
        if go_ahead:
            self.pathCtrl.SetValue(
                quote(relative_path('%s%sdummy.html' % (self.directory,
                                                        os.sep),
                                    path)))

    def GetPath(self):
        return self.pathCtrl.GetValue()

#---------------------------------------------------------------------------

class publishDlg(wxDialog):

    def __init__(self, parent, infile, project=None):
        wxDialog.__init__(self, parent, -1, title = 'Publish')
        btn_size = wxSize(75, 23)
        self.infile = infile
        self.project = project
        if self.project != None:
            self.directory = self.project.directory
        else:
            self.directory = os.path.dirname(infile)
        self.writer = 'HTML'
        # Writer
        wxStaticText(self, -1, 'Docutils-Writer:', wxPoint(8, 12))
        exitID = wxNewId()
        writers = publishers.keys()
        writers.sort()
        self.writerCtrl = wxChoice(self, exitID, (100, 10),
                                   choices = writers)
        self.writerCtrl.SetSelection(1)
        EVT_CHOICE(self, exitID, self.onChoiceWriter)

        # Output-directory
        wxStaticText(self, -1, 'Output-Directory:', wxPoint(8, 42))
        exitID = wxNewId()
        self.outdirCtrl = wxTextCtrl(self, exitID,
                                     pos = wxPoint(100, 40),
                                     size = wxSize(240, 21))
        self.outdirCtrl.SetValue(self.directory)
        exitID = wxNewId()
        self.btnSelOutdir = wxButton(self, exitID, 'Select',
                                     pos = wxPoint(350, 40),
                                     size = btn_size)
        EVT_BUTTON(self, exitID, self.onBtnOutdir)

        # Output-file
        wxStaticText(self, -1, 'Output-File:', wxPoint(8, 72))
        exitID = wxNewId()
        self.outfileCtrl = wxTextCtrl(self, exitID,
                                      pos = wxPoint(100, 70),
                                      size = wxSize(240, 21))
        self.outfileCtrl.SetValue(self.default_outfile())

        # OK and Cancel
        self.ok         = wxButton(self, wxID_OK, 'OK',
                                   pos = wxPoint(235-80,100),
                                   size = btn_size)
        self.cancel     = wxButton(self, wxID_CANCEL, 'Cancel',
                                   pos = wxPoint(235+5,100),
                                   size = btn_size)
        self.ok.SetDefault()
        self.Fit()

    def onBtnOutdir(self, event):
        dlg = wxDirDialog(self, 'Select output-directory:',
                          self.directory)
        if dlg.ShowModal() == wxID_OK:
            self.directory = dlg.GetPath()
            self.outdirCtrl.SetValue(self.directory)
        dlg.Destroy()

    def onChoiceWriter(self, event):
        self.writer = event.GetString()
        self.outfileCtrl.SetValue(self.default_outfile())

    def GetValues(self):
        return (self.outfileCtrl.GetValue(),
                self.outdirCtrl.GetValue(),
                self.writer)

    def default_outfile(self):
        outfile = self.outfileCtrl.GetValue()
        if outfile == '':
            outfile = os.path.splitext(os.path.basename(self.infile))[0] + publishers[self.writer][2]
        else:
            outfile = os.path.splitext(os.path.basename(outfile))[0] + publishers[self.writer][2]
        return outfile

#---------------------------------------------------------------------------

class toolsDlg(wxDialog):

    def __init__(self, parent):
        wxDialog.__init__(self, parent, -1, title = 'Configure Toolbox')
        self.tools = parent.tools
        self.parent = parent
        self.currentItem = None
        btn_size = wxSize(75, 23)
        self.il = wxImageList(16, 16)
        bmp = images.getToolBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        self.idx1 = self.il.Add(bmp)
        exitID = wxNewId()
        self.list = wxListCtrl(self, exitID,
                               wxDLG_PNT(self, 26, 10),
                               wxDLG_SZE(self, 400, 120),
                               wxLC_REPORT|wxSUNKEN_BORDER|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES)
        self.list.InsertColumn(0, "Tool")
        self.list.InsertColumn(1, "Name")
        self.list.InsertColumn(2, "Command")
        self.list.InsertColumn(3, "Initial Directory")
        self.list.SetImageList(self.il, wxIMAGE_LIST_SMALL)
        EVT_LIST_ITEM_SELECTED(self, exitID, self.on_item_selected)
        self.update_list()
        self.ok         = wxButton(self, wxID_OK, 'OK',
                                   pos = wxDLG_PNT(self, 195, 135),
                                   size = btn_size)
        exitID = wxNewId()
        bmp = images.getPlusBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 10),
                           wxSize(23, 23))
        b.SetToolTipString("Add tool")
        EVT_BUTTON(self, exitID, self.on_plus_btn)
        exitID = wxNewId()
        bmp = images.getPenBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 26),
                           wxSize(23, 23))
        b.SetToolTipString("Edit tool")
        EVT_BUTTON(self, exitID, self.on_edit_btn)
        exitID = wxNewId()
        bmp = images.getMinusBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 42),
                           wxSize(23, 23))
        b.SetToolTipString("Remove tool")
        EVT_BUTTON(self, exitID, self.on_minus_btn)
        self.ok.SetDefault()
        self.Fit()

    def add_tool(self, name, command, init_dir):
        if self.tools == {}:
            key = 1
        else:
            key = max(self.tools.keys())+1
        self.tools[key] = [0, name, command, init_dir]
        i = 1
        for tool in self.tools.values():
            tool[0] = i
            i = i+1
        self.update_list()
    
    def colourize_list(self):
        color = ('yellow', 'light blue')
        for i in range(self.list.GetItemCount()):
            item = self.list.GetItem(i)
            item.SetBackgroundColour(color[i%2])
            self.list.SetItem(item)

    def get_column_text(self, index, col):
        item = self.list.GetItem(index, col)
        return item.GetText()

    def get_tools(self):
        return self.tools

    def update_list(self):
        self.list.DeleteAllItems()
        items = self.tools.items()
        for x in range(len(items)):
            key, data = items[x]
            self.list.InsertImageStringItem(x, str(data[0]), self.idx1)
            self.list.SetStringItem(x, 1, data[1])
            self.list.SetStringItem(x, 2, data[2])
            self.list.SetStringItem(x, 3, data[3])
            self.list.SetItemData(x, key)
        for i in range(4):
            self.list.SetColumnWidth(i, wxLIST_AUTOSIZE)
        self.colourize_list()

    def on_edit_btn(self, event):
        go_ahead = 1
        if self.currentItem != None:
            item = self.currentItem
        else:
            go_ahead = 0
            customMsgBox(self, 'Select a tool first.', 'wakeup')
        if go_ahead:
            selected_tool = [int(self.list.GetItemText(self.currentItem)),
                             self.get_column_text(self.currentItem, 1),
                             self.get_column_text(self.currentItem, 2),
                             self.get_column_text(self.currentItem, 3)]
            dlg = editToolDlg(self, selected_tool)
            dlg.Centre()
            if dlg.ShowModal() == wxID_OK:
                new_values = dlg.get_values()
            else:
                go_ahead = 0
        if go_ahead:
            for key in self.tools.keys():
                if self.tools[key] == selected_tool:
                    self.tools[key][1] = new_values[0]
                    self.tools[key][2] = new_values[1]
                    self.tools[key][3] = new_values[2]
            self.update_list()

    def on_item_selected(self, event):
        self.currentItem = event.m_itemIndex

    def on_minus_btn(self, event):
        go_ahead = 1
        if self.currentItem != None:
            item = self.currentItem
        else:
            go_ahead = 0
            customMsgBox(self, 'Select a tool first.', 'wakeup')
        if go_ahead:
            selected_tool = [int(self.list.GetItemText(self.currentItem)),
                             self.get_column_text(self.currentItem, 1),
                             self.get_column_text(self.currentItem, 2),
                             self.get_column_text(self.currentItem, 3)]
            for key in self.tools.keys():
                if self.tools[key] == selected_tool:
                    del self.tools[key]
            i = 1
            for tool in self.tools.values():
                tool[0] = i
                i = i+1
            self.update_list()

    def on_plus_btn(self, event):
        dlg = editToolDlg(self)
        dlg.Centre()
        if dlg.ShowModal() == wxID_OK:
            tool = dlg.get_values()
            self.add_tool(tool[0],tool[1],tool[2])
        dlg.Destroy()

#---------------------------------------------------------------------------

class configEditDlg(wxDialog):

    def __init__(self, parent, configfile):
        wxDialog.__init__(self, parent, -1, title = 'docutils.conf')
        try:
            self.cfg = ConfigParser.ConfigParser()
            self.cfg.read(configfile)
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        if self.cfg.sections() == []:
            self.cfg.add_section('general')
            self.cfg.set('general','output_encoding','UTF-8')
            self.cfg.set('general','generator','1') 
            self.cfg.set('general','source_link','1') 
            self.cfg.set('general','datestamp','%Y-%m-%d %H:%M UTC') 
            self.cfg.add_section('html4css1 writer')
            self.cfg.set('html4css1 writer','stylesheet_path','')
        self.configfile = configfile
        self.currentItem = None
        btn_size = wxSize(75, 23)
        exitID = wxNewId()
        self.list = wxListCtrl(self, exitID,
                               wxDLG_PNT(self, 26, 10),
                               wxDLG_SZE(self, 230, 120),
                               wxLC_REPORT|wxSUNKEN_BORDER|wxLC_SINGLE_SEL|wxLC_HRULES|wxLC_VRULES)
        self.list.InsertColumn(0, "Section")
        self.list.InsertColumn(1, "Option")
        self.list.InsertColumn(2, "Value")
        EVT_LIST_ITEM_SELECTED(self, exitID, self.on_item_selected)
        self.update()
        self.ok         = wxButton(self, wxID_OK, 'OK',
                                   pos = wxDLG_PNT(self, 105, 135),
                                   size = btn_size)
        exitID = wxNewId()
        bmp = images.getPlusBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 10),
                           wxSize(23, 23))
        b.SetToolTipString("Add entry")
        EVT_BUTTON(self, exitID, self.on_plus_btn)
        exitID = wxNewId()
        bmp = images.getPenBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 26),
                           wxSize(23, 23))
        b.SetToolTipString("Edit entry")
        EVT_BUTTON(self, exitID, self.on_edit_btn)
        exitID = wxNewId()
        bmp = images.getMinusBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 42),
                           wxSize(23, 23))
        b.SetToolTipString("Remove entry")
        EVT_BUTTON(self, exitID, self.on_minus_btn)
        self.ok.SetDefault()
        self.Fit()

    def add(self, section, option, value):
        go_ahead = 1
        if self.cfg.has_option(section, option):
            customMsgBox(self, '%s, %s has already been set.\nUse the edit-' \
                         'button to change its value.' % (section, option), 'wakeup')
            go_ahead = 0
        if go_ahead:
            if not self.cfg.has_section(section):
                self.cfg.add_section(section)
            self.cfg.set(section, option, value)
            self.update()
    
    def colourize_list(self):
        color = ('Green Yellow', 'Medium Goldenrod')
        for i in range(self.list.GetItemCount()):
            item = self.list.GetItem(i)
            item.SetBackgroundColour(color[i%2])
            self.list.SetItem(item)

    def get_column_text(self, index, col):
        item = self.list.GetItem(index, col)
        return item.GetText()

    def update(self):
        f = open(self.configfile, 'wt')
        self.cfg.write(f)
        f.close()
        self.list.DeleteAllItems()
        x = 0
        for section in self.cfg.sections():
            for option in self.cfg.options(section):
                key, data = x, [section, option, self.cfg.get(section, option)]
                self.list.InsertStringItem(x, str(data[0]))
                self.list.SetStringItem(x, 1, data[1])
                self.list.SetStringItem(x, 2, data[2])
                self.list.SetItemData(x, key)
                x +=1
        for i in range(3):
            self.list.SetColumnWidth(i, wxLIST_AUTOSIZE)
        self.colourize_list()

    def on_edit_btn(self, event):
        go_ahead = 1
        if self.currentItem != None:
            item = self.currentItem
        else:
            go_ahead = 0
            customMsgBox(self, 'Select an item first.', 'wakeup')
        if go_ahead:
            select = [self.list.GetItemText(self.currentItem),
                      self.get_column_text(self.currentItem, 1),
                      self.get_column_text(self.currentItem, 2)]
            self.cfg.remove_option(select[0], select[1])
            dlg = editConfigEntryDlg(self, select)
            dlg.Centre()
            if dlg.ShowModal() == wxID_OK:
                section, option, value = dlg.get_values()
            else:
                go_ahead = 0
        if go_ahead:
            if not self.cfg.has_section(section):
                self.cfg.add_section(section)
            self.cfg.set(section, option, value)
            self.update()

    def on_item_selected(self, event):
        self.currentItem = event.m_itemIndex

    def on_minus_btn(self, event):
        go_ahead = 1
        if self.currentItem != None:
            item = self.currentItem
        else:
            go_ahead = 0
            customMsgBox(self, 'Select an item first.', 'wakeup')
        if go_ahead:
            section, option = [self.list.GetItemText(self.currentItem),
                               self.get_column_text(self.currentItem, 1)]
            self.cfg.remove_option(section, option)
            self.update()

    def on_plus_btn(self, event):
        dlg = editConfigEntryDlg(self)
        dlg.Centre()
        if dlg.ShowModal() == wxID_OK:
            new = dlg.get_values()
            self.add(new[0],new[1],new[2])
        dlg.Destroy()

#---------------------------------------------------------------------------

class editConfigEntryDlg(wxDialog):

    def __init__(self, parent, entry=['','','']):
        wxDialog.__init__(self, parent, -1, title = 'docutils.conf setting')
        wxStaticText(self, -1, 'Section:', wxPoint(18, 13))
        self.section = wxTextCtrl(self, -1, pos = wxPoint(70, 10),
                               size = wxSize(310, -1))
        wxStaticText(self, -1, 'Option:', wxPoint(18, 45))
        self.option = wxTextCtrl(self, -1, pos = wxPoint(70, 42),
                                 size = wxSize(310, -1))
        wxStaticText(self, -1, 'Value:', wxPoint(18, 77))
        self.value = wxTextCtrl(self, -1, pos = wxPoint(70, 74),
                                size = wxSize(310, -1))
        btn_size = wxSize(75, 23)
        self.ok         = wxButton(self, wxID_OK, 'OK',
                                   pos = wxPoint(210-90,110),
                                   size = btn_size)
        EVT_BUTTON(self, wxID_OK, self.on_btn_ok)
        self.cancel     = wxButton(self, wxID_CANCEL, 'Cancel',
                                   pos = wxPoint(210-5,110),
                                   size = btn_size)
        self.section.SetValue(entry[0])
        self.option.SetValue(entry[1])
        self.value.SetValue(entry[2])
        self.ok.SetDefault()
        self.Fit()

    def get_values(self):
        return self.section.GetValue(), self.option.GetValue(), self.value.GetValue()

    def on_btn_ok(self, event):
        go_ahead = 1
        for ctrl in (self.section, self.option):
            if ctrl.GetValue() == '':
                go_ahead = 0
                break
        if go_ahead:
            self.EndModal(event.GetId())
        else:
            customMsgBox(self, 'At least section and option have to be filled.', 'wakeup')

#---------------------------------------------------------------------------

class editToolDlg(wxDialog):

    def __init__(self, parent, tool=['','','','']):
        wxDialog.__init__(self, parent, -1, title = 'Tool')
        wxStaticText(self, -1, 'Name:', wxPoint(28, 13))
        self.name = wxTextCtrl(self, -1, pos = wxPoint(64, 10),
                               size = wxSize(322, -1),
                               validator = customValidator(TOOL_NAME))
        wxStaticText(self, -1, 'Command:', wxPoint(28, 45))
        self.command = wxTextCtrl(self, -1, pos = wxPoint(84, 42),
                                  size = wxSize(302, -1),
                                  validator = customValidator(NO_SEMICOLON))
        wxStaticText(self, -1, 'Initial Directory:', wxPoint(28, 77))
        self.init_dir = wxTextCtrl(self, -1, pos = wxPoint(105, 74),
                                   size = wxSize(282, -1),
                                   validator = customValidator(NO_SEMICOLON))
        btn_size = wxSize(75, 23)
        self.ok         = wxButton(self, wxID_OK, 'OK',
                                   pos = wxPoint(230-80,110),
                                   size = btn_size)
        EVT_BUTTON(self, wxID_OK, self.on_btn_ok)
        self.cancel     = wxButton(self, wxID_CANCEL, 'Cancel',
                                   pos = wxPoint(230+5,110),
                                   size = btn_size)
        self.name.SetValue(tool[1])
        self.command.SetValue(tool[2])
        self.init_dir.SetValue(tool[3])
        self.ok.SetDefault()
        self.Fit()

    def get_values(self):
        return self.name.GetValue(), self.command.GetValue(), self.init_dir.GetValue()

    def on_btn_ok(self, event):
        go_ahead = 1
        for ctrl in (self.name, self.command, self.init_dir):
            if ctrl.GetValue() == '':
                go_ahead = 0
                break
        if go_ahead:
            self.EndModal(event.GetId())
        else:
            customMsgBox(self, 'All fields have to be filled.', 'wakeup')

#---------------------------------------------------------------------------

PROJECT_NAME = 1
TOOL_NAME = 2
NO_SEMICOLON = 3
PROJECT_CHARS = string.letters + string.digits
TOOL_CHARS = PROJECT_CHARS + '$[]|<>,.:-_\\/(){} '

class customValidator(wxPyValidator):
    def __init__(self, flag=None, pyVar=None):
        wxPyValidator.__init__(self)
        self.flag = flag
        EVT_CHAR(self, self.OnChar)

    def Clone(self):
        return customValidator(self.flag)

    def Validate(self, win):
        tc = self.GetWindow()
        val = tc.GetValue()
        if self.flag == PROJECT_NAME:
            for x in val:
                if x not in PROJECT_CHARS:
                    return false

        elif self.flag == TOOL_NAME:
            for x in val:
                if x not in TOOL_CHARS:
                    return false

        elif self.flag == NO_SEMICOLON:
            for x in val:
                if x == ';':
                    return false

        return true

    def OnChar(self, event):
        key = event.KeyCode()
        if key < WXK_SPACE or key == WXK_DELETE or key > 255:
            event.Skip()
            return
        if self.flag == PROJECT_NAME and chr(key) in PROJECT_CHARS:
            event.Skip()
            return
        if self.flag == TOOL_NAME and chr(key) in TOOL_CHARS:
            event.Skip()
            return
        if self.flag == NO_SEMICOLON and chr(key) != ';':
            event.Skip()
            return
        if not wxValidator_IsSilent():
            wxBell()
        return

