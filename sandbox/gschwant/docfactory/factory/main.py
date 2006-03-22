#!/usr/bin/env python

"""conf.pth
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.3
"""

import browser, images, re, sys, os, time, ConfigParser, wx
from   wxPython.wx               import *
from   wxPython.lib.dialogs      import wxMultipleChoiceDialog, \
     wxScrolledMessageDialog
from   wxPython.lib.imagebrowser import ImageDialog
from   wxPython.help             import *
from   dialogs                   import *
from   controls                  import CustomStyledTextCtrl
from   controls                  import CustomTreeCtrl
from   controls                  import CustomStatusBar
from   docutilsadapter           import publish_document, get_errors, \
     get_rest_bibl_fields, publishers, publish_document
from   docutils.utils            import relative_path
from   urllib                    import quote
from   wxPython.lib.buttons      import *


#-------------------------------------------------------------------------
# global variables
#-------------------------------------------------------------------------

NAME = 'DocFactory'

try:
    factory_path = os.path.dirname(__file__)
except:
    factory_path = os.path.abspath(sys.path[0])

if not os.path.isdir(factory_path):
    factory_path = os.path.abspath(sys.path[0])

try: 
    #f = open(os.path.join(factory_path, 'conf.pth'))
    f = open(r'C:\conf.pth')
    DATA = f.readline().splitlines()[0]
    f.close()
except:
    DATA = os.path.join(factory_path, 'docfactory.dat')

# need some IDs
[wxID_WXNEWPROJ, wxID_WXNEWREST, wxID_WXOPENFILE, 
 wxID_WXREMFILE, wxID_WXSAVEFILE, wxID_WXEXITAPP, wxID_WXPUBL, wxID_WXPUBLALL,
 wxID_WXPROJSETTINGS, wxID_WXINSERTPATH, wxID_WXINSERTIMAGE,
 wxID_WXINSERTFIGURE, wxID_WXCLOSEFILE, wxID_WXVIEWEOLS, wxID_WXEOLSTOCR,
 wxID_WXEOLSTOLF, wxID_WXEOLSTOCRLF, wxID_WXEOLSTO, wxID_WXVIEWEDGE,
 wxID_WXCUTSELECTION, wxID_WXCOPYSELECTION, wxID_WXPASTESELECTION,
 wxID_WXUNDO, wxID_WXREDO, wxID_WXGOTO, wxID_WXSELECTALL, wxID_WXVIEWWS,
 wxID_WXDELETEPROJ, wxID_WXFONTSIZE, wxID_WXSMALLFONT, wxID_WXNORMALFONT,
 wxID_WXBIGFONT, wxID_WXINSERT, wxID_WXBTNBOLD, wxID_WXBTNITALIC,
 wxID_WXBTNLITERAL, wxID_WXBTNLINK, wxID_WXBTNIMAGE, wxID_WXBTNTOOL,
 wxID_WXINSERTHYPERLINK, wxID_WXBTNPASTE, wxID_WXBTNCOPY, wxID_WXRUNTOOL,
 wxID_WXBTNCUT, wxID_WXBTNREDO, wxID_WXBTNUNDO, wxID_WXBTNSAVE,
 wxID_WXBTNOPEN, wxID_WXBTNNEW, wxID_WXBTNPUBLISH, wxID_WXBTNABOUT,
 wxID_WXFINDREPLACE, wxID_WXBACKUPFILES] = map(lambda init_menubar: wxNewId(), range(53))

# Accelerator-Table for key commands
ACCEL = [(wxACCEL_NORMAL,WXK_F7,wxID_WXPUBL),
         (wxACCEL_NORMAL,WXK_F8,wxID_WXRUNTOOL),
         (wxACCEL_CTRL,ord('F'),wxID_WXFINDREPLACE),
         (wxACCEL_CTRL,ord('G'),wxID_WXGOTO),
         (wxACCEL_CTRL,ord('N'),wxID_WXNEWREST),
         (wxACCEL_CTRL,ord('O'),wxID_WXOPENFILE),
         (wxACCEL_CTRL,ord('S'),wxID_WXSAVEFILE),
         (wxACCEL_ALT,ord('X'),wxID_WXEXITAPP)]

#-------------------------------------------------------------------------

def get_file_extension(filename):
    return os.path.splitext(filename)[1]

#-------------------------------------------------------------------------

class DocProject:
    def __init__(self):
        self.name = ''
        self.directory = 'Nobody expects the spamish inquisition.'
        self.files = []

    def add(self, file):
        if file not in self.files:
            self.files.append(file)
        else:
            raise 'File is already part of this project.'

    def remove(self, file):
        if file in self.files:
            self.files.remove(file)

#-------------------------------------------------------------------------

class CustomLog(wxPyLog):
    def __init__(self, textCtrl, logTime=0):
        wxPyLog.__init__(self)
        self.tc = textCtrl
        self.logTime = logTime

    def DoLogString(self, message, timeStamp):
        if self.logTime:
            message = time.strftime('%X', time.localtime(timeStamp)) + \
                      ': ' + message
        self.tc.AppendText(message + '\n')

#-------------------------------------------------------------------------

class DocFactoryFrame(wxFrame):
    def __init__(self, projects=[], initial_file=None):
        wxFrame.__init__(
            self, NULL, -1, NAME, wxDefaultPosition, (800, 600),
            style=wxDEFAULT_FRAME_STYLE)
        self.Centre()
        self.projects = projects
        self.formats = {}
        self.files = []
        self.project = None
        self.editor = None
        self.activeitem = None
        self.imagedir = None
        self.language_code = 'en'

        # Application-Icon
        bmp = images.getLogoSmallBitmap()
        mask = wx.Mask(bmp, wx.WHITE)
        bmp.SetMask(mask)
        logoicon = wxEmptyIcon()
        logoicon.CopyFromBitmap(bmp)
        self.SetIcon(logoicon)

        self.init_preferences()
        self.init_menubar()
        self.init_toolbar()

        self.SetAcceleratorTable(wxAcceleratorTable(ACCEL))

        # splitter windows
        splitter = wxSplitterWindow(self, -1, style=wxNO_3D|wxSP_3D)
        splitter2 = wxSplitterWindow(splitter, -1, style=wxNO_3D|wxSP_3D)

        # Set up a log
        self.log = wxTextCtrl(splitter2, -1,
                              style = wxTE_MULTILINE|wxTE_READONLY|wxHSCROLL)

        # Set the wxWindows log target to be this textctrl
        wxLog_SetActiveTarget(CustomLog(self.log))

        # tree
        tID = wxNewId()
        self.tree = CustomTreeCtrl(splitter, tID, style=wxTR_HAS_BUTTONS |
                               wxTR_HAS_VARIABLE_ROW_HEIGHT)
        EVT_TREE_ITEM_ACTIVATED(self.tree, tID, self.on_tree_item_activated)

        # make an image list for tree
        self.im0 = self.im1 = self.im2 = -1
        self.il = wxImageList(16, 16)
        self.im0 = self.il.Add(images.getLogoSmallBitmap())
        self.im1 = self.il.Add(images.getProjectBitmap())
        self.im2 = self.il.Add(images.getFile1Bitmap())
        self.tree.SetImageList(self.il)

        self.init_tree()
        
        # Create a Notebook
        tID = wxNewId()
        self.nb = wxNotebook(splitter2, tID, style=wxCLIP_CHILDREN)
        EVT_NOTEBOOK_PAGE_CHANGED(self.nb, tID, self.on_notebook_page_changed)

        # editor-page
        self.InitEditorPage()

        # create statusbar
        self.sb = CustomStatusBar(self)
        self.SetStatusBar(self.sb)

        # initial file
        if initial_file != None:
            self.load_initial_file(initial_file)

        self.Show(1)
        
        # add the windows to the splitter and split it.
        splitter2.SplitHorizontally(self.nb, self.log)
        splitter.SplitVertically(self.tree, splitter2)

        splitter.SetSashPosition(180, 1)
        splitter.SetMinimumPaneSize(20)
        splitter2.SetSashPosition(450, 1)
        splitter2.SetMinimumPaneSize(20)

        # Some global state variables.
        self.projectdirty = 0

        EVT_CLOSE(self, self.on_close_window)

    def init_menubar(self):
        self.mainmenu = wxMenuBar()
        mainwindow = self

        # File
        menu=wxMenu()
        menu.Append(wxID_WXNEWREST, 'New\tCtrl+N',
                    'Create a new txt-file and add to project')
        EVT_MENU(self, wxID_WXNEWREST, self.on_file_new)
        menu.Append(wxID_WXOPENFILE, 'Open...\tCtrl+O',
                    'Open a file')
        EVT_MENU(self, wxID_WXOPENFILE, self.on_file_open)
        menu.Append(wxID_WXCLOSEFILE, 'Close',
                    'Close file')
        EVT_MENU(self, wxID_WXCLOSEFILE, self.on_file_close)
        menu.Enable(wxID_WXCLOSEFILE, 0)
        menu.Append(wxID_WXSAVEFILE, 'Save\tCtrl+S', 'Save file now')
        EVT_MENU(self, wxID_WXSAVEFILE, self.on_file_save)
        menu.Enable(wxID_WXSAVEFILE, 0)
        menu.Append(wxID_WXREMFILE, 'Remove From Project',
                    'Remove file from project')
        EVT_MENU(self, wxID_WXREMFILE, self.on_file_remove)
        menu.Enable(wxID_WXREMFILE, 0)
        menu.AppendSeparator()
        menu.Append(wxID_WXPUBL, 'Publish...\tF7',
                    'Publish file')
        EVT_MENU(self, wxID_WXPUBL, self.on_publish)
        menu.Enable(wxID_WXPUBL, 0)
        menu.AppendSeparator()
        menu.Append(wxID_WXEXITAPP, 'Exit\tAlt+X', 'Exit program')
        EVT_MENU(self, wxID_WXEXITAPP, self.on_app_exit)
        self.mainmenu.Append (menu, '&File')

        # Edit
        menu=wxMenu()
        menu.Append(wxID_WXUNDO, 'Undo\tCtrl-Z', 'Undo')
        EVT_MENU(self, wxID_WXUNDO, self.on_undo)
        menu.Enable(wxID_WXUNDO, 0)
        menu.Append(wxID_WXREDO, 'Redo\tCtrl-Y', 'Redo')
        EVT_MENU(self, wxID_WXREDO, self.on_redo)
        menu.Enable(wxID_WXREDO, 0)
        menu.AppendSeparator()
        menu.Append(wxID_WXCUTSELECTION, 'Cut\tCtrl-X', 'Cut')
        EVT_MENU(self, wxID_WXCUTSELECTION, self.on_cut)
        menu.Enable(wxID_WXCUTSELECTION, 0)
        menu.Append(wxID_WXCOPYSELECTION, 'Copy\tCtrl-C', 'Copy')
        EVT_MENU(self, wxID_WXCOPYSELECTION, self.on_copy)
        menu.Enable(wxID_WXCOPYSELECTION, 0)
        menu.Append(wxID_WXPASTESELECTION, 'Paste\tCtrl-V', 'Paste')
        EVT_MENU(self, wxID_WXPASTESELECTION, self.on_paste)
        menu.Enable(wxID_WXPASTESELECTION, 0)
        menu.Append(wxID_WXSELECTALL, 'Select all\tCtrl-A', 'Select the entire file')
        EVT_MENU(self, wxID_WXSELECTALL, self.on_select_all)
        menu.Enable(wxID_WXSELECTALL, 0)
        menu.AppendSeparator()
        submenu=wxMenu()
        submenu.Append(wxID_WXINSERTFIGURE, 'Figure',
                       'Insert a figure')
        EVT_MENU(self, wxID_WXINSERTFIGURE, self.on_insert_figure)
        submenu.Append(wxID_WXINSERTHYPERLINK, 'Hyperlink', 'Insert a hyperlink')
        EVT_MENU(self, wxID_WXINSERTHYPERLINK, self.on_btn_hyperlink)
        submenu.Append(wxID_WXINSERTIMAGE, 'Image', 'Insert an image')
        EVT_MENU(self, wxID_WXINSERTIMAGE, self.on_insert_image)
        submenu.Append(wxID_WXINSERTPATH, 'Path', 'Insert a path')
        EVT_MENU(self, wxID_WXINSERTPATH, self.on_insert_path)
        menu.AppendMenu(wxID_WXINSERT, 'Insert', submenu)
        menu.Enable(wxID_WXINSERT, 0)
        menu.AppendSeparator()
        submenu=wxMenu()
        submenu.Append(wxID_WXEOLSTOCR,'CR',
                       'Change all end of line characters to CR (\\r)')
        EVT_MENU(self, wxID_WXEOLSTOCR, self.on_eols_to_cr)
        submenu.Append(wxID_WXEOLSTOLF,'LF',
                       'Change all end of line characters to LF (\\n)')
        EVT_MENU(self, wxID_WXEOLSTOLF, self.on_eols_to_lf)
        submenu.Append(wxID_WXEOLSTOCRLF,'CRLF',
                       'Change all end of line characters to LF (\\r\\n)')
        EVT_MENU(self, wxID_WXEOLSTOCRLF, self.on_eols_to_crlf)
        menu.AppendMenu(wxID_WXEOLSTO, 'Change all EOLs to', submenu)
        menu.Enable(wxID_WXEOLSTO, 0)
        menu.AppendSeparator()
        menu.Append(wxID_WXFINDREPLACE, 'Find && Replace...\tCtrl-F', 'Find and replace')
        EVT_MENU(self, wxID_WXFINDREPLACE, self.on_findreplace_show)
        EVT_COMMAND_FIND(self, -1, self.on_find)
        EVT_COMMAND_FIND_NEXT(self, -1, self.on_find)
        EVT_COMMAND_FIND_REPLACE(self, -1, self.on_find)
        EVT_COMMAND_FIND_REPLACE_ALL(self, -1, self.on_find)
        EVT_COMMAND_FIND_CLOSE(self, -1, self.on_find_close)
        menu.Append(wxID_WXGOTO, 'Goto line...\tCtrl-G', 'Goto a specific line in the file')
        EVT_MENU(self, wxID_WXGOTO, self.on_goto)
        menu.Enable(wxID_WXGOTO, 0)
        self.mainmenu.Append (menu, '&Edit')

        # Project
        menu=wxMenu()
        menu.Append(wxID_WXNEWPROJ, 'New', 'Create a new project')
        EVT_MENU(self, wxID_WXNEWPROJ, self.on_project_new)
        menu.Append(wxID_WXDELETEPROJ, 'Delete...',
                    'Delete one or more projects')
        EVT_MENU(self, wxID_WXDELETEPROJ, self.on_project_delete)
        menu.Append(wxID_WXPUBLALL, 'Publish...',
                    'Publish all txt-files of active project')
        EVT_MENU(self, wxID_WXPUBLALL, self.on_publish_all)
        menu.Enable(wxID_WXPUBLALL, 0)
        menu.AppendSeparator()
        menu.Append(wxID_WXPROJSETTINGS, 'Project Settings...',
                    'Edit project settings')
        EVT_MENU(self, wxID_WXPROJSETTINGS, self.on_project_settings)
        if len(self.projects) == 0:
            menu.Enable(wxID_WXDELETEPROJ, 0)
        menu.Enable(wxID_WXPROJSETTINGS, 0)
        self.mainmenu.Append (menu, 'Pr&oject')

        self.init_tools()
        
        # Preferences
        menu=wxMenu()
        menu.Append(wxID_WXVIEWEOLS, 'View EOL markers',
                    'Show or hide end-of-line markers', wxITEM_CHECK)
        EVT_MENU(self, wxID_WXVIEWEOLS, self.on_view_eols)
        menu.Check(wxID_WXVIEWEOLS, self.preferences['eol_markers'])
        menu.Append(wxID_WXVIEWEDGE, 'View right edge indicator',
                    'Toggle display of the right edge indicator (75 characters)',
                    wxITEM_CHECK)
        EVT_MENU(self, wxID_WXVIEWEDGE, self.on_view_edge)
        menu.Check(wxID_WXVIEWEDGE, self.preferences['right_edge_indicator'])
        menu.Append(wxID_WXVIEWWS, 'View whitespace',
                    'Show or hide whitespace', wxITEM_CHECK)
        EVT_MENU(self, wxID_WXVIEWWS, self.on_view_ws)
        menu.Check(wxID_WXVIEWWS, self.preferences['whitespace'])
        menu.AppendSeparator()
        submenu=wxMenu()
        submenu.Append(wxID_WXSMALLFONT,'Small',
                       'Reduce font', wxITEM_RADIO)
        EVT_MENU(self, wxID_WXSMALLFONT, self.on_font_small)
        submenu.Append(wxID_WXNORMALFONT,'Normal',
                       'Restore font', wxITEM_RADIO)
        EVT_MENU(self, wxID_WXNORMALFONT, self.on_font_normal)
        submenu.Append(wxID_WXBIGFONT,'Big',
                       'Magnify font', wxITEM_RADIO)
        EVT_MENU(self, wxID_WXBIGFONT, self.on_font_big)
        if self.preferences['fontsize'] == 'small':
            submenu.Check(wxID_WXSMALLFONT, 1)
        elif self.preferences['fontsize'] == 'big':
            submenu.Check(wxID_WXBIGFONT, 1)
        else:
            submenu.Check(wxID_WXNORMALFONT, 1)
        menu.AppendMenu(wxID_WXFONTSIZE, 'Fontsize', submenu)
        menu.AppendSeparator()
        menu.Append(wxID_WXBACKUPFILES, 'Backup files',
                    'Backup files as [filename].bak', wxITEM_CHECK)
        EVT_MENU(self, wxID_WXBACKUPFILES, self.on_backup_files)
        menu.Check(wxID_WXBACKUPFILES, self.preferences['backup_files'])
        self.mainmenu.Append(menu, '&Preferences')

        # Toolbox
        menu=wxMenu()
        exitID=wxNewId()
        menu.Append(exitID, 'Configure', 'Configure toolbox')
        EVT_MENU(self, exitID, self.on_configure_tools)
        menu.Append(wxID_WXRUNTOOL, 'Open...\tF8', 'Open toolbox')
        EVT_MENU(self, wxID_WXRUNTOOL, self.on_run_tool)
        self.mainmenu.Append (menu, '&Toolbox')
        
        # Help
        menu=wxMenu()
        exitID=wxNewId()
        menu.Append(exitID, 'About', 'About')
        EVT_MENU(self, exitID, self.on_help_about)
        self.mainmenu.Append (menu, '&Help')

        self.SetMenuBar(self.mainmenu)

    def init_preferences(self):
        self.preferences = {}
        if os.path.exists(DATA):
            try:
                cfg = ConfigParser.ConfigParser()
                cfg.read(DATA)
                if cfg.has_section('preferences'):
                    for pref in cfg.options('preferences'):
                        if pref in ('eol_markers', 'right_edge_indicator', \
                                    'whitespace', 'backup_files'):
                            self.preferences[pref] = cfg.getboolean('preferences', pref)
                        else:
                            self.preferences[pref] = cfg.get('preferences', pref)
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        if not self.preferences.has_key('eol_markers'):
            self.preferences['eol_markers'] = 0
        if not self.preferences.has_key('right_edge_indicator'):
            self.preferences['right_edge_indicator'] = 1
        if not self.preferences.has_key('whitespace'):
            self.preferences['whitespace'] = 0
        if not self.preferences.has_key('fontsize'):
            self.preferences['fontsize'] = 'normal'
        if not self.preferences.has_key('backup_files'):
            self.preferences['backup_files'] = 0

    def init_toolbar(self):
        self.toolbar = tb = self.CreateToolBar(wxTB_HORIZONTAL|wxTB_FLAT|wxNO_BORDER)
        bmp = images.getNewBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNNEW, 'New', bmp, shortHelp='New',
                        longHelp='Create a new file')
        EVT_TOOL(tb, wxID_WXBTNNEW, self.on_file_new)
        bmp = images.getOpenBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNOPEN, 'Open', bmp, shortHelp='Open',
                        longHelp='Open an existing file')
        EVT_TOOL(tb, wxID_WXBTNOPEN, self.on_file_open)
        bmp = images.getSaveBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNSAVE, 'Save', bmp, shortHelp='Save',
                        longHelp='Save the active file')
        EVT_TOOL(tb, wxID_WXBTNSAVE, self.on_file_save)
        tb.EnableTool(wxID_WXBTNSAVE, 0)
        tb.AddSeparator()
        bmp = images.getCutBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNCUT, 'Cut', bmp, shortHelp='Cut',
                        longHelp='Cut the selection and put it on the Clipboard')
        EVT_TOOL(tb, wxID_WXBTNCUT, self.on_cut)
        tb.EnableTool(wxID_WXBTNCUT, 0)
        bmp = images.getCopyBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNCOPY, 'Copy', bmp, shortHelp='Copy',
                        longHelp='Copy the selection and put it on the Clipboard')
        EVT_TOOL(tb, wxID_WXBTNCOPY, self.on_copy)
        tb.EnableTool(wxID_WXBTNCOPY, 0)
        bmp = images.getPasteBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNPASTE, 'Paste', bmp, shortHelp='Paste',
                        longHelp='Insert Clipboard contents')
        EVT_TOOL(tb, wxID_WXBTNPASTE, self.on_paste)
        tb.EnableTool(wxID_WXBTNPASTE, 0)
        tb.AddSeparator()
        bmp = images.getUndoBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNUNDO, 'Undo', bmp, shortHelp='Undo',
                        longHelp='Undo the last action')
        EVT_TOOL(tb, wxID_WXBTNUNDO, self.on_undo)
        tb.EnableTool(wxID_WXBTNUNDO, 0)
        bmp = images.getRedoBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNREDO, 'Redo', bmp, shortHelp='Redo',
                        longHelp='Redo the previously undone action')
        EVT_TOOL(tb, wxID_WXBTNREDO, self.on_redo)
        tb.EnableTool(wxID_WXBTNREDO, 0)
        tb.AddSeparator()
        bmp = images.getLinkBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNLINK, 'Hyperlink', bmp,
                        shortHelp='Insert hyperlink',
                        longHelp='Insert hyperlink')
        EVT_TOOL(tb, wxID_WXBTNLINK, self.on_btn_hyperlink)
        tb.EnableTool(wxID_WXBTNLINK, 0)
        bmp = images.getImageBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNIMAGE, 'Image', bmp,
                        shortHelp='Insert image',
                        longHelp='Insert image')
        EVT_TOOL(tb, wxID_WXBTNIMAGE, self.on_btn_image)
        tb.EnableTool(wxID_WXBTNIMAGE, 0)
        tb.AddSeparator()
        bmp = images.getPublishBitmap()
        mask = wx.Mask(bmp, wxRED)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNPUBLISH, 'Publish', bmp, shortHelp='Publish file',
                        longHelp='Publish file')
        EVT_TOOL(tb, wxID_WXBTNPUBLISH, self.on_publish)
        tb.EnableTool(wxID_WXBTNPUBLISH, 0)
        tb.AddSeparator()
        bmp = images.getToolBitmap()
        mask = wx.Mask(bmp, wxBLUE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNTOOL, 'Toolbox', bmp, shortHelp='Open toolbox',
                        longHelp='Open toolbox')
        EVT_TOOL(tb, wxID_WXBTNTOOL, self.on_run_tool)
        tb.AddSeparator()
        bmp = images.getBoldBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNBOLD, 'Bold', bmp, shortHelp='Bold', longHelp='Bold')
        EVT_TOOL(tb, wxID_WXBTNBOLD, self.on_format_word)
        tb.EnableTool(wxID_WXBTNBOLD, 0)
        bmp = images.getItalicBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNITALIC, 'Italic', bmp, shortHelp='Italic', longHelp='Italic')
        EVT_TOOL(tb, wxID_WXBTNITALIC, self.on_format_word)
        tb.EnableTool(wxID_WXBTNITALIC, 0)
        bmp = images.getPreBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNLITERAL, 'literal', bmp, shortHelp='Literal', longHelp='Literal')
        EVT_TOOL(tb, wxID_WXBTNLITERAL, self.on_format_word)
        tb.EnableTool(wxID_WXBTNLITERAL, 0)
        if wxPlatform != '__WXMAC__':
            tb.AddSeparator()
            self.formats = {'Title'     : '=', 'Subtitle'  : '-',
                            'Heading 1' : '=', 'Heading 2' : '-',
                            'Heading 3' : '~', 'Heading 4' : '`'}
            formats = self.formats.keys()
            formats.sort()
            exitID=wxNewId()
            self.combobox_format = wxComboBox(tb, exitID, '',
                                              choices = formats,
                                              style=wxCB_DROPDOWN|wxCB_READONLY)
            tb.AddControl(self.combobox_format)
            EVT_COMBOBOX(tb, exitID, self.on_format_paragraph)
        tb.AddSeparator()
        bmp = images.getAboutBitmap()
        mask = wx.Mask(bmp, wxWHITE)
        bmp.SetMask(mask)
        tb.AddLabelTool(wxID_WXBTNABOUT, 'About', bmp, shortHelp='About',
                        longHelp='Display program information')
        EVT_TOOL(tb, wxID_WXBTNABOUT, self.on_help_about)
        tb.Realize()

    def init_tools(self):
        self.tools = {}
        if os.path.exists(DATA):
            try:
                cfg = ConfigParser.ConfigParser()
                cfg.read(DATA)
                if cfg.has_section('tools'):
                    for i in range(len(cfg.options('tools'))):
                        self.tools[i+1] = [i+1] \
                                          + cfg.get('tools', str(i+1)).split(';')
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
    
    def init_tree(self):
        self.tree.DeleteAllItems()
        self.root = self.tree.AddRoot('Workspace', self.im0)
        self.tree.SetPyData(self.root, None)
        for proj in self.projects:
            child = self.tree.AppendItem(self.root, proj.name, self.im1)
            self.tree.SetPyData(child, None)
            for file in proj.files:
                last = self.tree.AppendItem(child, file, self.im2)
                self.tree.SetPyData(last, None)
        self.tree.SortChildren(self.root)
        for file in self.files:
            last = self.tree.AppendItem(self.root, file, self.im2)
            self.tree.SetPyData(last, None)
        self.project = None
        self.activateMenuItemsProjectOpen(0)
        self.activateMenuItemsFileSelected(0)
        self.activeitem = self.root
        self.tree.SelectItem(self.activeitem)
        self.tree.Expand(self.activeitem)
        if self.editor != None:
            self.nb.SetPageText(0, 'Editor')
            self.editor.Clear()
            self.editor.Enable(0)
            self.activateMenuItemsFileSelected(0)
            if self.nb.GetPageCount() > 1:
                self.nb.DeletePage(1)
        
    def InitEditorPage(self):
        # init editor
        edID = wxNewId()
        self.editor = CustomStyledTextCtrl(self.nb, edID, self.log)
        self.editor.SetViewEOL(self.preferences['eol_markers'])
        self.editor.SetEdgeMode(self.preferences['right_edge_indicator'])
        self.editor.SetViewWhiteSpace(self.preferences['whitespace'])
        if self.preferences['fontsize'] == 'small':
            self.editor.SetZoom(-2)
        elif self.preferences['fontsize'] == 'big':
            self.editor.SetZoom(2)
        else:
            self.editor.SetZoom(0)
        self.editor.Clear()
        self.editor.Enable(0)
        self.nb.AddPage(self.editor, 'Editor')
        self.nb.SetSelection(0)

    # --------------------------------------------------------------------
    # handlers
    # --------------------------------------------------------------------

    def activateMenuItemsProjectOpen(self, value):
        menu = self.mainmenu.GetMenu(self.mainmenu.FindMenu('Project'))
        if len(self.projects) > 0 and value:
            menu.Enable(wxID_WXDELETEPROJ, value)
        menu.Enable(wxID_WXPROJSETTINGS, value)
        menu.Enable(wxID_WXPUBLALL, value)

    def activateMenuItemsFileSelected(self, value):
        menu = self.mainmenu.GetMenu(self.mainmenu.FindMenu('File'))
        if self.project != None:
            menu.Enable(wxID_WXREMFILE, value)
        else:
            menu.Enable(wxID_WXREMFILE, 0)
        menu.Enable(wxID_WXCLOSEFILE, value)
        menu.Enable(wxID_WXSAVEFILE, value)
        menu.Enable(wxID_WXPUBL, value)
        menu = self.mainmenu.GetMenu(self.mainmenu.FindMenu('Edit'))
        menu.Enable(wxID_WXUNDO, value)
        menu.Enable(wxID_WXREDO, value)
        menu.Enable(wxID_WXCUTSELECTION, value)
        menu.Enable(wxID_WXCOPYSELECTION, value)
        menu.Enable(wxID_WXPASTESELECTION, value)
        menu.Enable(wxID_WXSELECTALL, value)
        menu.Enable(wxID_WXINSERT, value)
        menu.Enable(wxID_WXEOLSTO, value)
        menu.Enable(wxID_WXFINDREPLACE, value)
        menu.Enable(wxID_WXGOTO, value)
        self.toolbar.EnableTool(wxID_WXBTNSAVE, value)
        self.toolbar.EnableTool(wxID_WXBTNLINK, value)
        self.toolbar.EnableTool(wxID_WXBTNIMAGE, value)
        self.toolbar.EnableTool(wxID_WXBTNBOLD, value)
        self.toolbar.EnableTool(wxID_WXBTNITALIC, value)
        self.toolbar.EnableTool(wxID_WXBTNLITERAL, value)
        self.toolbar.EnableTool(wxID_WXBTNPASTE, value)
        self.toolbar.EnableTool(wxID_WXBTNCOPY, value)
        self.toolbar.EnableTool(wxID_WXBTNCUT, value)
        self.toolbar.EnableTool(wxID_WXBTNREDO, value)
        self.toolbar.EnableTool(wxID_WXBTNUNDO, value)
        self.toolbar.EnableTool(wxID_WXBTNPUBLISH, value)

    def CheckEditorChanges(self):
        go_ahead = 1
        if self.editor != None:
            if self.editor.IsModified:
                dlg=wxMessageDialog(self, 'Save changes?', NAME,
                                    wxYES_NO | wxCANCEL | wxICON_QUESTION)
                result = dlg.ShowModal()
                if result == wxID_YES:
                    file = self.tree.GetItemText(self.activeitem)
                    go_ahead = self.editor.SaveFile(file, self.preferences['backup_files'])
                if result == wxID_CANCEL:
                    go_ahead = 0
                dlg.Destroy()
        return go_ahead

    def delete_project(self, project):
        if project in self.projects:
            self.projects.remove(project)
        try:
            self.save_projects()
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        self.init_tree()

    def htmlfile(self, file, dir):
        htmlfile = os.path.join(dir,
                                os.path.splitext(os.path.basename(file))[0] \
                                + '.html')
        return htmlfile

    def insert_hyperlink(self):
        item = self.activeitem
        itemimage = self.tree.GetItemImage(item)
        file = self.tree.GetItemText(item)
        if self.project != None:
            dir = self.project.directory
        else:
            dir = os.path.dirname(file)
        go_ahead = 1
        dlg = hyperlinkDlg(self, directory=dir, project=self.project)
        dlg.Centre()
        if dlg.ShowModal() == wxID_OK:
            target = dlg.GetPath()
        else:
            go_ahead = 0
        dlg.Destroy()
        if go_ahead:
            selection = self.editor.GetSelectedText()
            curpos = self.editor.GetCurrentPos()
            if selection != '':
                self.editor.BeginUndoAction()
                selection = '`%s`' % selection
                self.editor.ReplaceSelection(selection + '_')
                endpos = self.editor.GetTextLength()
                self.editor.GotoPos(endpos)
                text = '\n\n.. _%s: %s' % (selection, target)
                self.editor.ReplaceSelection(text)
                self.editor.GotoPos(curpos + 3)
                self.editor.EndUndoAction()
            else:
                text = '\n\n.. _``: %s\n' % target
                self.editor.ReplaceSelection(text)
                self.editor.GotoPos(curpos + 7)

    def insert_image(self, directive):
        item = self.activeitem
        itemimage = self.tree.GetItemImage(item)
        file = self.tree.GetItemText(item)
        if self.imagedir == None:
            if self.project != None:
                dir = self.project.directory
            else:
                dir = os.path.dirname(file)
        else:
            dir = self.imagedir
        dlg = ImageDialog(self, dir)
        dlg.Centre()
        go_ahead = 1
        if dlg.ShowModal() == wxID_OK:
            target = dlg.GetFile()
            self.imagedir = os.path.dirname(target)
        else:
            go_ahead = 0
        dlg.Destroy()
        if go_ahead:
            if self.project != None:
                dir = self.project.directory
            else:
                dir = os.path.dirname(file)
                dlg = wxDirDialog(self, 'Calculate path relative'
                                  ' to which outputdirectory?', dir)
                if dlg.ShowModal() == wxID_OK:
                    dir = dlg.GetPath()
                else:
                    go_ahead = 0
                dlg.Destroy()
        if go_ahead:
            selection = self.editor.GetSelectedText()
            if selection != '' and directive == 'image':
                self.editor.BeginUndoAction()
                curpos = self.editor.GetCurrentPos()
                selection = '|%s|' % selection
                self.editor.ReplaceSelection(selection)
                endpos = self.editor.GetTextLength()
                self.editor.GotoPos(endpos)
                text = '\n\n.. %s %s:: %s' % (selection, directive,
                                              quote(relative_path(self.htmlfile(file,dir),
                                                                  target)))
                self.editor.ReplaceSelection(text)
                self.editor.GotoPos(curpos + 2)
                self.editor.EndUndoAction()
            else:
                text = '\n\n.. %s:: %s\n\n' % (directive,
                                               quote(relative_path(self.htmlfile(file,dir),
                                                                   target)))
                self.editor.ReplaceSelection(text)

    def load_initial_file(self, file):
        if os.path.exists(file):
            try:
                self.files.append(file)
                dir = ''
                parent = self.root
                self.open_file_in_editor(file)
                last = self.tree.AppendItem(parent, file, self.im2)
                self.tree.SetPyData(last, None)
                self.tree.SetItemBold(self.activeitem, 0)
                self.tree.SetItemTextColour(self.activeitem, wxBLACK)
                self.activeitem = last
                self.tree.SetItemBold(self.activeitem, 1)
                self.tree.SetItemTextColour(self.activeitem, wxBLUE)
                self.tree.EnsureVisible(self.activeitem)
                self.tree.SelectItem(self.activeitem)
                parent = self.tree.GetItemParent(self.activeitem)
                self.tree.SetItemBold(parent, 1)
                self.tree.SetItemTextColour(parent, wxBLUE)
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        else:
            customMsgBox(self, 'Can not find %s.' % file, 'error')

    def open_file_in_editor(self, file):
        if self.nb.GetPageCount() > 1:
            self.nb.DeletePage(1)
        self.nb.SetSelection(0)
        self.editor.LoadFile(file)
        self.set_editor_language_code(file)
        self.nb.SetPageText(0, 'Editor: %s' %
                            os.path.basename(file))
        self.editor.Enable(1)
        self.activateMenuItemsFileSelected(1)
        self.editor.IsModified = 0

    def project_save(self):
        try:
            self.save_projects()
            self.projectdirty = 0
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def publishFile(self, infile, outfile, outdir, writer):
        wxBeginBusyCursor()
        try:
            self.log.Clear()
            t = time.localtime(time.time())
            st = time.strftime('%d-%b-%Y, %H:%M:%S', t)
            wxLogMessage('%s: Publishing %s' % (st, writer))
            wxLogMessage('SOURCE: %s' % infile)
            outfile_fullpath = os.path.join(outdir, outfile)
            wxLogMessage('DESTINATION: %s' % outfile_fullpath)
            if outfile_fullpath == infile:
                customMsgBox(self, 'Destination and source are identical.'
                             '\nNo processing.', 'wakeup')
                warning_lines = error_lines = []
            else:
                try:
                    publish_document(writer, infile, outfile, outdir)
                finally:
                    warning_lines, error_lines = get_errors(self.log.GetValue())
                    linecount = self.editor.GetLineCount()
                    self.editor.MarkerDeleteAll(0)
                    self.editor.MarkerDeleteAll(1)
                    if warning_lines != []:
                        for line in warning_lines:
                            if line < linecount:
                                self.editor.MarkerAdd(line, 0)
                        self.editor.GotoLine(warning_lines[-1])
                    if error_lines != []:
                        for line in error_lines:
                            if line < linecount:
                                self.editor.MarkerAdd(line, 1)
                        self.editor.GotoLine(error_lines[-1])
                    self.editor.IsModified = 0
            if os.path.exists(outfile_fullpath):
                if self.nb.GetPageCount() > 1:
                    self.nb.DeletePage(1)
                if get_file_extension(outfile) in ['.html', '.htm']:
                    # init html-viewer page
                    if wxPlatform == '__WXMSW__':
                        htmlprv = browser.IEHtmlPanel(self.nb, self, self.log,
                                                      'file://%s' % quote(outfile_fullpath))
                    else:
                        htmlprv = browser.HtmlPanel(self.nb, self, self.log,
                                                    outfile_fullpath)
                    self.nb.AddPage(htmlprv, 'HTML-Viewer: %s'
                                    % outfile)
                    if warning_lines == error_lines == []:
                        self.nb.SetSelection(1)
            t = time.localtime(time.time())
            st = time.strftime('%d-%b-%Y, %H:%M:%S: ', t)
            wxLogMessage('%sFinished.' % st)
        finally:
            wxEndBusyCursor()

    def save_preferences(self):
        cfg = ConfigParser.ConfigParser()
        try:
            cfg.read(DATA)
            if not cfg.has_section('preferences'):
                cfg.add_section('preferences')
            for pref in self.preferences.keys():
                cfg.set('preferences', pref,
                        self.preferences[pref])
            f = open(DATA, 'wt')
            cfg.write(f)
            f.close()
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def save_projects(self):
        cfg = ConfigParser.ConfigParser()
        cfg.read(DATA)
        for section in cfg.sections():
            if section[:19] == 'docfactory_project:':
                cfg.remove_section(section)
        for project in self.projects:
            section = 'docfactory_project: %s' % project.name
            cfg.add_section(section)
            cfg.set(section, 'outputdirectory', project.directory)
            files = ''
            for file in project.files:
                files = '%s;%s' % (files, file)
            if len(files) > 1:
                files = files[1:]
            cfg.set(section, 'files', files)
        f = open(DATA, 'wt')
        cfg.write(f)
        f.close()

    def set_editor_language_code(self, file=None):
        language_code = 'en'
        if self.project != None:
            directory = self.project.directory
        else:
            directory = os.path.split(os.path.abspath(file))[0]
        docutils_conf = os.path.join(directory, 'docutils.conf')
        if os.path.exists(docutils_conf):
            try:
                cfg = ConfigParser.ConfigParser()
                cfg.read(docutils_conf)
                if cfg.has_option('general', 'language_code'):
                    language_code = cfg.get('general', 'language_code')
            except:
                print '%s:\n%s\n%s' % sys.exc_info()
        self.editor.bibliographic_fields = get_rest_bibl_fields(language_code)        

    # --------------------------------------------------------------------
    # event handlers
    # --------------------------------------------------------------------

    def on_app_exit(self, event):
        self.Close()

    def on_backup_files(self, event):
        self.preferences['backup_files'] = not self.preferences['backup_files']

    def on_btn_image(self, event):
        self.insert_image('image')

    def on_close_window(self, event):
        go_ahead = 1
        if self.projectdirty:
            dlg=wxMessageDialog(self, 'Save project?', NAME,
                                wxYES_NO | wxCANCEL | wxICON_QUESTION)
            result = dlg.ShowModal()
            if result == wxID_YES:
                self.project_save()
            if result == wxID_CANCEL:
                go_ahead = 0
            dlg.Destroy()

        if go_ahead and self.CheckEditorChanges():
            self.Destroy()

    def on_configure_tools(self, event):
        dlg = toolsDlg(self)
        dlg.Centre()
        if dlg.ShowModal() == wxID_OK:
            self.tools = dlg.get_tools()
        dlg.Destroy()
        cfg = ConfigParser.ConfigParser()
        try:
            cfg.read(DATA)
            if cfg.has_section('tools'):
                cfg.remove_section('tools')
            cfg.add_section('tools')
            for tool in self.tools.values():
                cfg.set('tools', str(tool[0]),
                        '%s;%s;%s' % (tool[1],tool[2],tool[3]))
            f = open(DATA, 'wt')
            cfg.write(f)
            f.close()
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def on_copy(self, event):
        self.editor.Copy()

    def on_cut(self, event):
        self.editor.Cut()

    def on_eols_to_cr(self, event):
        wxBeginBusyCursor()
        self.editor.ConvertEOLs(1)
        wxEndBusyCursor()

    def on_eols_to_lf(self, event):
        wxBeginBusyCursor()
        self.editor.ConvertEOLs(2)
        wxEndBusyCursor()

    def on_eols_to_crlf(self, event):
        wxBeginBusyCursor()
        self.editor.ConvertEOLs(0)
        wxEndBusyCursor()

    def on_file_close(self, event):
        if self.CheckEditorChanges():
            if self.nb.GetPageCount() > 1:
                self.nb.DeletePage(1)
            self.nb.SetPageText(0, 'Editor')
            self.editor.Clear()
            self.editor.Enable(0)
            self.activateMenuItemsFileSelected(0)
            item = self.activeitem
            parent = self.tree.GetItemParent(item)
            self.tree.SetItemBold(item, 0)
            self.tree.SetItemTextColour(item, wxBLACK)
            if parent == self.root:
                self.files.remove(self.tree.GetItemText(item))
                self.tree.Delete(item)
            self.activeitem = parent
            self.tree.SelectItem(self.activeitem)

    def on_file_open(self, event):

        go_ahead = 1
        
        if self.CheckEditorChanges():

            if self.project != None:
                dir = self.project.directory
                parent = self.tree.GetItemParent(self.activeitem)
                if parent == self.root:
                    parent = self.activeitem
            else:
                dir = ''
                parent = self.root

            wildcard = 'Text (*.txt)|*.txt|' \
                       'All files (*.*)|*.*'

            dlg = wxFileDialog (self, 'Open file',
                                dir, '', wildcard,
                                wxOPEN|wxFILE_MUST_EXIST)
            if dlg.ShowModal() == wxID_OK:
                file = dlg.GetPath()
                if parent == self.root:
                    if file not in self.files:
                        self.files.append(file)
                    else:
                        customMsgBox(self, '%s already in workspace.' % file,
                                     'wakeup')
                        go_ahead = 0
                else:
                    if file not in self.project.files:
                        self.project.add(file)
                        self.project_save()
                    else:
                        customMsgBox(self, '%s already part of project "%s".'
                                     % (file, self.project.name),
                                     'wakeup')
                        go_ahead = 0
            else:
                go_ahead = 0
            dlg.Destroy()

            if go_ahead:
                self.open_file_in_editor(file)
                last = self.tree.AppendItem(parent, file, self.im2)
                self.tree.SetPyData(last, None)
                self.tree.SetItemBold(self.activeitem, 0)
                self.tree.SetItemTextColour(self.activeitem, wxBLACK)
                self.activeitem = last
                self.tree.SetItemBold(self.activeitem, 1)
                self.tree.SetItemTextColour(self.activeitem, wxBLUE)
                self.tree.EnsureVisible(self.activeitem)
                self.tree.SelectItem(self.activeitem)
                parent = self.tree.GetItemParent(self.activeitem)
                self.tree.SetItemBold(parent, 1)
                self.tree.SetItemTextColour(parent, wxBLUE)

    def on_file_new(self, event):
        go_ahead = 1

        if self.project != None:
            dir = self.project.directory
        else:
            dir = ''

        dlg = wxFileDialog (self, 'Create new file',
                            dir, '', '*.txt',
                            wxSAVE|wxOVERWRITE_PROMPT)
        if dlg.ShowModal() == wxID_OK:
            file = dlg.GetPath()
        else:
            go_ahead = 0
        dlg.Destroy()

        if go_ahead:
            dlg = wxTextEntryDialog(
                self, 'Enter a title for this document:', 'Title', '',
                wxOK | wxCANCEL)
            dlg.Centre()
            if dlg.ShowModal() == wxID_OK:
                title = dlg.GetValue()
            else:
                go_ahead = 0
            dlg.Destroy()

        if go_ahead:
            try:
                f = open(file, 'w')
                f.write(len(title)*'='+'\n')
                f.write(title+'\n')
                f.write(len(title)*'='+'\n')
                f.close()
                if self.project != None:
                    self.project.add(file)
                    self.project_save()
                    parent = self.tree.GetItemParent(self.activeitem)
                    if parent == self.root:
                        parent = self.activeitem
                else:
                    self.files.append(file)
                    parent = self.root
                self.open_file_in_editor(file)
                last = self.tree.AppendItem(parent, file, self.im2)
                self.tree.SetPyData(last, None)
                self.tree.SetItemBold(self.activeitem, 0)
                self.tree.SetItemTextColour(self.activeitem, wxBLACK)
                self.activeitem = last
                self.tree.SetItemBold(self.activeitem, 1)
                self.tree.SetItemTextColour(self.activeitem, wxBLUE)
                self.tree.EnsureVisible(self.activeitem)
                self.tree.SelectItem(self.activeitem)
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(),
                             'error')
        dlg.Destroy()

    def on_file_remove(self, event):
        item = self.tree.GetSelection()
        file = self.tree.GetItemText(item)
        self.activeitem = self.tree.GetItemParent(item)
        self.tree.Delete(item)
        self.project.remove(file)
        self.project_save()
        self.nb.SetPageText(0, 'Editor')
        self.editor.Clear()
        self.editor.Enable(0)
        self.activateMenuItemsFileSelected(0)
        dlg=wxMessageDialog(
            self, 'Delete file %s from disk?' % file, NAME,
            wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION)
        result = dlg.ShowModal()
        if result == wxID_YES:
            try:
                os.remove(file)
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(),
                             'error')
        dlg.Destroy()

    def on_file_save(self, event):
        go_ahead = 1
        file = self.tree.GetItemText(self.activeitem)
        wxLogMessage('Saving %s.' % file)
        go_ahead = self.editor.SaveFile(file, self.preferences['backup_files'])
        if go_ahead:
            if self.nb.GetPageCount() > 1:
                self.nb.DeletePage(1)

    def on_publish(self, event):
        go_ahead = 1
        item = self.activeitem
        file = self.tree.GetItemText(item)
        if self.editor.IsModified:
            wxLogMessage('Saving %s.' % file)
            wxBeginBusyCursor()
            go_ahead = self.editor.SaveFile(file, self.preferences['backup_files'])
            wxEndBusyCursor()
        if go_ahead and os.path.exists(file):
            dlg = publishDlg(self, infile=file, project=self.project)
            dlg.Centre()
            if dlg.ShowModal() == wxID_OK:
                outfile, outdir, writer = dlg.GetValues()
            else:
                go_ahead = 0
            dlg.Destroy()
            if go_ahead:
                self.publishFile(file, outfile, outdir, writer)

    def on_publish_all(self, event):
        go_ahead = 1
        item = self.activeitem
        file = self.tree.GetItemText(item)
        if self.editor.IsModified:
            wxLogMessage('Saving %s.' % file)
            wxBeginBusyCursor()
            go_ahead = self.editor.SaveFile(file, self.preferences['backup_files'])
            wxEndBusyCursor()
        if go_ahead:
            writers = publishers.keys()
            writers.sort()
            dlg = wxSingleChoiceDialog(self, 'Please select a writer:',
                                       'Publish "%s"' % self.project.name,
                                       writers, wxOK|wxCANCEL)
            dlg.SetSelection(writers.index('HTML'))
            dlg.Centre()
            if dlg.ShowModal() == wxID_OK:
                writer = writers[dlg.GetSelection()]
            else:
                go_ahead = 0
            dlg.Destroy()
            if go_ahead:
                self.log.Clear()
                files_to_publish = []
                for file in self.project.files:
                    if os.path.splitext(file)[1] == '.txt':
                        files_to_publish.append(file)
                max = len(files_to_publish)
                dlg = wxProgressDialog('Publishing "%s"' % self.project.name,
                                       '',
                                       max,
                                       self,
                                       wxPD_CAN_ABORT | wxPD_APP_MODAL)
                keepGoing = true
                count = 0
                error_files = []
                while keepGoing and count < max:
                    infile = files_to_publish[count]
                    outfile = '%s%s' % (os.path.splitext(os.path.split(infile)[1])[0],
                                        publishers[writer][2])
                    outdir = self.project.directory
                    keepGoing = dlg.Update(count, outfile)
                    t = time.localtime(time.time())
                    st = time.strftime('%d-%b-%Y, %H:%M:%S', t)
                    wxLogMessage('\n%s: Publishing %s' % (st, writer))
                    wxLogMessage('SOURCE: %s' % infile)
                    outfile_fullpath = os.path.join(outdir, outfile)
                    wxLogMessage('DESTINATION: %s' % outfile_fullpath)
                    try:
                        publish_document(writer, infile, outfile, outdir)
                    except:
                        wxLogMessage('ERROR: %s (%s)' % sys.exc_info()[:2])
                        error_files.append((infile, sys.exc_info()[1]))
                    count = count + 1
                dlg.Destroy()
                if error_files != []:
                    msg = 'Due to severe errors the following files have ' \
                          'not been published properly: \n'
                    for file in error_files:
                        msg = '%s\n%s\n(%s)\n' % (msg, file[0], file[1])
                    msg = '%s\nHINT: You should try to publish the files ' \
                          'in question separately first. If that works, ' \
                          'you can try to publish all files again.' % msg
                    dlg = wxScrolledMessageDialog(self, msg, "WARNING!")
                    dlg.Centre()
                    dlg.ShowModal()
                    dlg.Destroy()

    def on_find(self, event):
        et = event.GetEventType()
        if et == wxEVT_COMMAND_FIND_REPLACE or et == wxEVT_COMMAND_FIND_REPLACE_ALL:
            replacetxt = event.GetReplaceString()
        self.sb.SetStatusText('', 0)
        findtxt = event.GetFindString()
        length = len(findtxt)
        lastpos = self.editor.GetTextLength()
        flags = event.GetFlags()
        if flags in (1, 3, 5, 7):
            if flags == 7:
                # whole word / match case
                regexp = re.compile(r'\b%s\b' % findtxt)
            elif flags == 5:
                # no whole word / match case
                regexp = re.compile(r'%s' % findtxt)
            elif flags == 3:
                # whole word / no match case
                regexp = re.compile(r'\b%s\b' % findtxt, re.IGNORECASE)
            else:
                # no whole word / no match case
                regexp = re.compile(findtxt, re.IGNORECASE)
        else:
            regexp = None
            print 'Unknown combination of flags.'
        if regexp != None:
            if et == wxEVT_COMMAND_FIND_REPLACE_ALL:
                wxBeginBusyCursor()
                self.editor.BeginUndoAction()
                origtxt = self.editor.GetText()
                if regexp.search(origtxt) != None:
                    self.editor.SetText(regexp.sub(replacetxt, origtxt))
                    self.sb.SetStatusText('Replaced.', 0)
                else:
                    self.sb.SetStatusText('No match found.', 0)
                self.editor.EndUndoAction()
                wxEndBusyCursor()
            else:
                currpos = self.editor.GetCurrentPos()
                if et == wxEVT_COMMAND_FIND_REPLACE and currpos != 0:
                    self.editor.ReplaceSelection(replacetxt)
                    currpos = self.editor.GetCurrentPos()
                    lastpos = self.editor.GetTextLength()
                origtxt = self.editor.GetTextRange(currpos, lastpos)
                position = len(regexp.split(origtxt)[0])
                startpos = currpos + position
                if  startpos < lastpos:
                    self.editor.GotoPos(startpos + len(regexp.findall(origtxt)[0]))
                    self.editor.SetAnchor(startpos)
                else:
                    self.sb.SetStatusText('Can not find "%s". Next search will '
                                          'start from beginning.' % findtxt, 0)
                    self.editor.GotoPos(0)

    def on_find_close(self, event):
        event.GetDialog().Destroy()

    def on_findreplace_show(self, event):
        data = wxFindReplaceData()
        dlg = wxFindReplaceDialog(self, data, 'Find & Replace',
                                  wxFR_REPLACEDIALOG)
        dlg.data = data
        dlg.Show(1)
        
    def on_font_small(self, event):
        self.preferences['fontsize'] = 'small'
        self.editor.SetZoom(-2)
        self.save_preferences()

    def on_font_normal(self, event):
        self.preferences['fontsize'] = 'normal'
        self.editor.SetZoom(0)
        self.save_preferences()

    def on_font_big(self, event):
        self.preferences['fontsize'] = 'big'
        self.editor.SetZoom(2)
        self.save_preferences()

    def on_format_paragraph(self, event):
        self.nb.SetSelection(0)
        line_no = self.editor.GetCurrentLine()
        self.editor.GotoLine(line_no)
        startpos = self.editor.GetCurrentPos()
        line = self.editor.GetLine(line_no)
        endpos = startpos + len(line)
        self.editor.SetSelection(startpos, endpos)
        format = event.GetString()
        if format in ['Title','Subtitle']:
            overline = (len(line)-1)*self.formats[format]+'\n'
        else:
            overline = ''
        underline = (len(line)-1)*self.formats[format]+'\n'
        line_below = self.editor.GetLine(line_no+1)
        if line != underline and line_below != underline:
            self.editor.ReplaceSelection('\n%s%s%s\n' % (overline,
                                                         line.strip(' '),
                                                         underline))
        self.editor.GotoPos(startpos)

    def on_format_word(self, event):
        self.nb.SetSelection(0)
        selection = self.editor.GetSelectedText()
        if selection != '':
            if event.GetId() == wxID_WXBTNBOLD:
                symbol = '**'
            elif event.GetId() == wxID_WXBTNITALIC:
                symbol = '*'
            elif event.GetId() == wxID_WXBTNLITERAL:
                symbol = '``'
            else:
                symbol = ''
            selection = selection.replace('*','').replace('`','').strip()
            selection = '%s%s%s' % (symbol,selection,symbol)
            self.editor.ReplaceSelection(selection)

    def on_goto(self, event):
        values = []
        for i in range(self.editor.GetLineCount()+1)[1:]:
            values.append(str(i))
        dlg = wxSingleChoiceDialog(self, 'Select a line number:', 'Goto line...',
                                   values, wxOK|wxCANCEL)
        if dlg.ShowModal() == wxID_OK:
            self.editor.GotoLine(int(dlg.GetStringSelection())-1)
        dlg.Destroy()

    def on_help_about(self, event):
        """
        Event handler for menu
        option *Help -> About*.
        """
        dlg = aboutDlg(self)
        try:
            dlg.Centre()
            dlg.ShowModal()
        finally:
            dlg.Destroy()

    def on_insert_figure(self, event):
        self.insert_image('figure')

    def on_btn_hyperlink(self, event):
        self.insert_hyperlink()
        
    def on_insert_image(self, event):
        self.insert_image('image')

    def on_insert_path(self, event):
        item = self.activeitem
        itemimage = self.tree.GetItemImage(item)
        file = self.tree.GetItemText(item)
        if self.project != None:
            dir = self.project.directory
        else:
            dir = os.path.dirname(file)
        dlg = wxFileDialog (self, 'Choose file',
                            dir, '', '*.*',
                            wxOPEN|wxFILE_MUST_EXIST)
        go_ahead = 1
        if dlg.ShowModal() == wxID_OK:
            target = dlg.GetPath()
        else:
            go_ahead = 0
        if go_ahead:
            if self.project != None:
                dir = self.project.directory
            else:
                dir = os.path.dirname(file)
                dlg = wxDirDialog(self, 'Calculate path relative'
                                  ' to which outputdirectory?', dir)
                if dlg.ShowModal() == wxID_OK:
                    dir = dlg.GetPath()
                else:
                    go_ahead = 0
                dlg.Destroy()
        if go_ahead:
            self.editor.ReplaceSelection(
                quote(relative_path(self.htmlfile(file,dir), target)))
        dlg.Destroy()

    def on_notebook_page_changed(self, event):
        event.Skip()

    def on_paste(self, event):
        self.editor.Paste()
        
    def on_project_delete(self, event):
        available_projects = []
        for project in self.projects:
            available_projects.append(project.name)
        if available_projects != []:
            available_projects.sort()
            dlg = wxMultipleChoiceDialog(self, 'Select the projects which you want to' \
                                         '\ndelete or press "Cancel" to abort.',
                                         'Delete Projects',
                                         available_projects)
            dlg.Centre()
            if dlg.ShowModal() == wxID_OK:
                selection = dlg.GetValueString()
                for project_name in selection:
                    for project in self.projects:
                        if project.name == project_name:
                            self.delete_project(project)
            dlg.Destroy()
        else:
            customMsgBox(self, 'Sorry, I don\'t remember any projects.',
                         'info')

    def on_project_new(self, event):
        go_ahead = 1

        other_project_names = []
        for project in self.projects:
            other_project_names.append(project.name)

        if self.CheckEditorChanges():
            project = DocProject()
            dlg = projectSettingsDlg(self, project, other_project_names)
            dlg.Centre()
            if dlg.ShowModal() == wxID_CANCEL:
                go_ahead = 0
            else:
                self.project = project
                self.project.name, self.project.directory = dlg.getValues()
            dlg.Destroy()

        if go_ahead:
            try:
                self.projects.append(self.project)
                self.save_projects()
                self.init_tree()
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def on_project_settings(self, event):
        go_ahead = 1

        other_project_names = []
        for project in self.projects:
            if project.name != self.project.name:
                other_project_names.append(project.name)

        dlg = projectSettingsDlg(self, self.project, other_project_names)
        dlg.Centre()
        if dlg.ShowModal() == wxID_CANCEL:
            go_ahead = 0
        else:
            name, directory = dlg.getValues()
            self.set_editor_language_code()
        dlg.Destroy()

        if go_ahead:
            sort_tree_new = 0
            if self.project.name != name:
                self.projectdirty = 1
                sort_tree_new = 1
                for project in self.projects:
                    if project.name == self.project.name:
                        project.name = name
                self.project.name = name
            if self.project.directory != directory:
                self.project.directory = directory
                self.projectdirty = 1

        if self.projectdirty:
            self.project_save()
            if sort_tree_new:
                self.init_tree()

    def on_redo(self, event):
        self.editor.Redo()

    def on_run_tool(self, event):
        go_ahead = 1
        choices = []
        for tool in self.tools.values():
            choices.append('%s: %s' % (tool[0],tool[1]))
        dlg = wxSingleChoiceDialog(self, 'Select a tool...', 'Toolbox',
                                   choices, wxOK|wxCANCEL)
        dlg.Centre()
        if dlg.ShowModal() == wxID_OK:
            tool_id=int(dlg.GetStringSelection().split(':')[0])
            for key in self.tools.keys():
                if self.tools[key][0] == tool_id:
                    tool_key = key
                    break
        else:
            go_ahead = 0
        dlg.Destroy()
        if go_ahead:
            self.log.Clear()
            tool = self.tools[tool_key]
            item = self.activeitem
            file = self.tree.GetItemText(item)
            curdir = os.getcwd()
            try:
                replmts = {'$[FileDir]': os.path.split(file)[0],
                           '$[FilePath]': file,
                           '$[FileName]': os.path.split(file)[1],
                           '$[FileBase]': os.path.splitext(os.path.basename(file))[0],
                           '$[ProjectDir]': self.project.directory}
                command = tool[2]
                for str in replmts.keys():
                    command = command.replace(str, replmts[str])
                directory = tool[3]
                for str in replmts.keys():
                    directory = directory.replace(str, replmts[str])
            except:
                go_ahead = 0
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        if go_ahead:
            t = time.localtime(time.time())
            st = time.strftime('%d-%b-%Y, %H:%M:%S', t)
            wxLogMessage('%s: %s' % (st, tool[1]))
            wxLogMessage('COMMAND: %s\nDIRECTORY: %s' % (command, directory))
            try:
                os.chdir(directory)
                os.system(command)
                t = time.localtime(time.time())
                st = time.strftime('%d-%b-%Y, %H:%M:%S', t)
                wxLogMessage('%s: Finished.' % st)
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
            os.chdir(curdir)

    def on_select_all(self, event):
        self.editor.SelectAll()

    def on_tree_item_activated(self, event):
        item=event.GetItem()
        go_ahead = 1

        if item != self.activeitem \
           and self.tree.GetItemImage(self.activeitem) == self.im2:
            go_ahead = self.CheckEditorChanges()

        if go_ahead:
            self.nb.SetSelection(0)
            if item != self.activeitem:
                if self.activeitem != self.root:
                    olditemparent = self.tree.GetItemParent(self.activeitem)
                    self.tree.SetItemBold(olditemparent, 0)
                    self.tree.SetItemTextColour(olditemparent, wxBLACK)
                self.tree.SetItemBold(self.activeitem, 0)
                self.tree.SetItemTextColour(self.activeitem, wxBLACK)
                self.activeitem = item
                self.tree.SetItemBold(item, 1)
                if item != self.root:
                    self.tree.SetItemTextColour(item, wxBLUE)
            else:
                go_ahead = 0

        if go_ahead:
            itemparent = self.tree.GetItemParent(item)
            itemimage = self.tree.GetItemImage(item)

            # item is file:
            if itemimage == self.im2:
                # file is part of project:
                if self.tree.GetItemImage(itemparent) == self.im1:
                    self.tree.SetItemBold(itemparent, 1)
                    self.tree.SetItemTextColour(itemparent, wxBLUE)
                    project_name = self.tree.GetItemText(itemparent)
                    for project in self.projects:
                        if project.name == project_name:
                            self.project = project
                    self.activateMenuItemsProjectOpen(1)
                # file is not part of project:
                else:
                    self.project = None
                    self.activateMenuItemsProjectOpen(0)
                file=self.tree.GetItemText(item)
                if os.path.exists(file):
                    self.open_file_in_editor(file)
                else:
                    self.editor.Clear()
                    self.editor.Enable(0)
                    self.nb.SetPageText(0, 'Editor')
                    self.activateMenuItemsFileSelected(0)
                    parent = self.tree.GetItemParent(item)
                    self.tree.SetItemBold(item, 0)
                    self.tree.SetItemTextColour(item, wxBLACK)
                    self.activeitem = parent
                    dlg=wxMessageDialog(self, '%s does not exist.\nRemove '
                                        'from project?' % file,
                                        NAME, wxYES_NO | wxICON_QUESTION)
                    if dlg.ShowModal() == wxID_YES:
                        self.tree.Delete(item)
                        self.project.remove(file)
                        self.project_save()
                    dlg.Destroy()
                    self.tree.SelectItem(self.activeitem)

            # item is project:
            elif itemimage == self.im1:
                project_name = self.tree.GetItemText(item)
                for project in self.projects:
                    if project.name == project_name:
                        self.project = project
                self.activateMenuItemsProjectOpen(1)
                self.nb.SetPageText(0, 'Editor')
                self.editor.Clear()
                self.editor.Enable(0)
                self.activateMenuItemsFileSelected(0)

            # item is neither file nor project:
            else:
                self.nb.SetPageText(0, 'Editor')
                self.editor.Clear()
                self.editor.Enable(0)
                self.project = None
                self.activateMenuItemsProjectOpen(0)
                self.activateMenuItemsFileSelected(0)

    def on_undo(self, event):
        self.editor.Undo()

    def on_view_eols(self, event):
        self.preferences['eol_markers'] = not self.preferences['eol_markers']
        self.editor.SetViewEOL(self.preferences['eol_markers'])
        self.save_preferences()
        
    def on_view_edge(self, event):
        self.preferences['right_edge_indicator'] = not self.preferences['right_edge_indicator']
        self.editor.SetEdgeMode(self.preferences['right_edge_indicator'])
        self.save_preferences()

    def on_view_ws(self, event):
        self.preferences['whitespace'] = not self.preferences['whitespace'] 
        self.editor.SetViewWhiteSpace(self.preferences['whitespace'])
        self.save_preferences()

#---------------------------------------------------------------------------

class FactoryApp(wxApp):
    def OnInit(self):
        provider = wxSimpleHelpProvider()
        wxHelpProvider_Set(provider)
        self.projects = []
        if self.init_projects():
            wxInitAllImageHandlers()
            if len(sys.argv) > 1:
                frame = DocFactoryFrame(self.projects, sys.argv[1])
            else:
                frame = DocFactoryFrame(self.projects)
            self.SetTopWindow(frame)
            return 1
        else:
            return 0

    def init_projects(self):
        if os.path.exists(DATA):
            try:
                cfg = ConfigParser.ConfigParser()
                cfg.read(DATA)
                for section in cfg.sections():
                    if section[:19] == 'docfactory_project:':
                        project = DocProject()
                        project.name = section.split(': ')[1]
                        project.directory = cfg.get(section, 'outputdirectory')
                        if cfg.has_option(section, 'files'):
                            project.files = cfg.get(section, 'files').split(';')
                            if project.files == ['']:
                                project.files = []
                        self.projects.append(project)
            except:
                f = open('error.txt', 'w')
                f.write('%s:\n%s\n%s' % sys.exc_info())
                f.close()
                return 0
        return 1

#--------------------------------------------------------------------------

def main():
    app = FactoryApp(0)
    app.MainLoop()

#--------------------------------------------------------------------------

if __name__ == '__main__':
    main()

#--------------------------------------------------------------------------
