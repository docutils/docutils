"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.2.5
"""

from   wxPython.lib.throbber         import Throbber
from   wxPython.wx                   import *
from   wxPython.help                 import *
from   wxPython.lib.filebrowsebutton import DirBrowseButton
from   docutils.utils                import relative_path
from   urllib                        import quote
from   docutilsadapter               import language_codes, publishers
import images, os, string, ConfigParser, stylesheets, throbimages

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
        mask = wxMaskColour(bmp, wxWHITE)
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
                            '>>> release:           0.2.5',
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
        self.config = self.loadconf()
        self.invalid_names = invalid_names

        #--------------------------------------------------
        # Create a Notebook
        tID = wxNewId()
        self.nb = wxNotebook(self, tID, style=wxCLIP_CHILDREN)
        #EVT_NOTEBOOK_PAGE_CHANGED(self.nb, tID, self.on_notebook_page_changed)

        #--------------------------------------------------
        # DocFactory Page
        self.df = wxPanel(self, -1)

        # Help Button
        wxContextHelpButton(self.df, pos = wxPoint(370, 8),
                            size = wxSize(23, 23))

        # Output Directory
        wxStaticText(self.df, -1, 'Output-Directory', wxPoint(8, 42))
        exitID = wxNewId()
        self.dirCtrl    = wxTextCtrl(self.df, exitID,
                                     pos = wxPoint(96, 40),
                                     size = wxSize(240, 21))
        self.dirCtrl.SetHelpText('This is the default ' + \
                                 'directory for output-files.')
        exitID = wxNewId()
        self.btnSelDir  = wxButton(self.df, exitID, 'Select',
                                   pos = wxPoint(344, 40),
                                   size = wxSize(75, 23))
        EVT_BUTTON(self, exitID, self.onBtnSelDir)

        # Title
        wxStaticText(self.df, -1, 'Title', wxPoint(8, 74))
        exitID = wxNewId()
        self.nameCtrl = wxTextCtrl(self.df, exitID,
                                   pos = wxPoint(96, 72),
                                   size = wxSize(240, 21),
                                   validator = customValidator(PROJECT_NAME))
        self.nameCtrl.SetHelpText('Enter a title for your project here.')

        #--------------------------------------------------
        # Docutils Page
        self.du = wxPanel(self, -1)

        # Help Button
        wxContextHelpButton(self.du, pos = wxPoint(370, 8),
                            size = wxSize(23, 23))        

        # Language Code
        wxStaticText(self.du, -1, 'Language-Code', wxPoint(8, 10))
        exitID = wxNewId()
        if not self.config.options['language_code'] in language_codes:
            language_codes.append(self.config.options['language_code'])
        language_codes.sort()
        self.languageCodeCtrl = wxChoice(self.du, exitID, pos = wxPoint(97, 8),
                                         choices = language_codes)
        self.languageCodeCtrl.SetStringSelection(self.config.options['language_code'])
        self.languageCodeCtrl.SetHelpText('Language code of your documents.')
        EVT_CHOICE(self, exitID, self.onChoiceLanguageCode)
        
        # Stylesheet
        wxStaticText(self.du, -1, 'Stylesheet', wxPoint(8, 42))
        exitID = wxNewId()
        self.styCtrl    = wxTextCtrl(self.du, exitID,
                                     pos = wxPoint(96, 40),
                                     size = wxSize(240, 21))
        self.styCtrl.SetHelpText('Path to stylesheet.')
        exitID = wxNewId()
        self.btnSelSty  = wxButton(self.du, exitID, 'Select',
                                   pos = wxPoint(344, 40),
                                   size = wxSize(75, 23), style = 0)
        EVT_BUTTON(self, exitID, self.onBtnSelSty)

        # Output Encoding
        wxStaticText(self.du, -1, 'Output-Encoding', wxPoint(8, 74))
        exitID = wxNewId()
        self.outencCtrl    = wxTextCtrl(self.du, exitID,
                                        self.config.options['output_encoding'],
                                        pos = wxPoint(96, 72),
                                        size = wxSize(115, 21))
        self.outencCtrl.SetHelpText('Encoding of output.')

        # Datestamp
        wxStaticText(self.du, -1, 'Datestamp', wxPoint(8, 106))
        exitID = wxNewId()
        self.datestampCtrl = wxTextCtrl(self.du, exitID,
                                        pos = wxPoint(96, 104),
                                        size = wxSize(115, 21))
        self.datestampCtrl.SetHelpText('Include a time/datestamp in ' + \
                                       'the document footer. Contains ' + \
                                       'a format string for time.strftime.')

        # TOC-Backlinks
        exitID = wxNewId()
        self.tocliCtrl = wxRadioBox(self.du, exitID, 'TOC-Backlinks',
                                    wxPoint(224, 79), wxDefaultSize,
                                    ['entry','top','none'],
                                    3, wxRA_SPECIFY_COLS)
        self.tocliCtrl.SetHelpText('Enable backlinks from section ' + \
                                   'titles to table of contents ' + \
                                   'entries ("entry"), to the top ' + \
                                   'of the TOC ("top"), or disable ("none").')

        # Footnote References
        exitID = wxNewId()
        self.footrefCtrl = wxRadioBox(self.du, exitID,
                                      'Footnote References',
                                      wxPoint(224, 131),
                                      wxDefaultSize,
                                      ['superscript','brackets'],
                                      1, wxRA_SPECIFY_COLS)
        self.footrefCtrl.SetHelpText('Format for footnote references.')

        # More Options via Checklistbox
        optionList = ['Compact Lists',
                      'Embed Stylesheet',
                      'Footnote-Backlinks',
                      'Generator',
                      'Source-Link',
                      'Use LaTeX-TOC'
                      ]
        exitID = NewId()
        self.optionsCtrl = wxCheckListBox(self.du, exitID, wxPoint(50, 136),
                                          wxSize(161, 90), optionList)
        self.optionsCtrl.SetHelpText('Set more Docutils-options here.')

        #--------------------------------------------------
        # Fill controls with initial values
        self.init_ctrls()
        
        #--------------------------------------------------
        # Sizing of Notebook
        self.df.Fit()
        self.du.Fit()
        size_of_df = self.df.GetSize()
        size_of_du = self.du.GetSize()
        self.nb.SetSize(wxSize(max(size_of_df[0],size_of_du[0])+15,
                         max(size_of_df[1],size_of_du[1])+15))

        #--------------------------------------------------
        # Add pages to notebook
        self.nb.AddPage(self.df, 'DocFactory')
        self.nb.AddPage(self.du, 'Docutils')
        self.nb.SetSelection(0)

        #--------------------------------------------------
        # OK- and Cancel-Buttons
        size_of_nb = self.nb.GetSize()
        ok_position = wxPoint((size_of_nb[0]/2)-80,size_of_nb[1]+10)
        cancel_position = wxPoint((size_of_nb[0]/2)+5,size_of_nb[1]+10)
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
        dlg = wxDirDialog(self, 'Choose a directory for output files', dir)
        if dlg.ShowModal() == wxID_OK:
            directory = dlg.GetPath()
            self.project.directory = directory
            newconfig = os.path.join(directory, 'docutils.conf')
            if newconfig != self.configfile:
                self.configfile = newconfig
                self.config = self.loadconf()
                self.init_ctrls()
        dlg.Destroy()

    def onBtnSelSty(self, event):
        dir = self.dirCtrl.GetValue()
        if not os.path.isdir(dir):
            customMsgBox(self, 'Invalid Output-Directory.', 'wakeup')
        else:
            wildcard = 'Stylesheet-Files (*.css, *.tex)|*.css;*.tex|' \
                       'All files (*.*)|*.*'
            dlg = wxFileDialog(self, 'Choose a stylesheet',
                               dir, '', wildcard,
                               wxOPEN|wxFILE_MUST_EXIST)
            if dlg.ShowModal() == wxID_OK:
                stylesheetpath = relative_path(self.configfile,
                                               dlg.GetPath())
                self.styCtrl.SetValue(stylesheetpath)
            dlg.Destroy()

    def onChoiceLanguageCode(self, event):
        self.config.options['language_code'] = event.GetString()
        
    def getValues(self):
        return(self.nameCtrl.GetValue(), self.dirCtrl.GetValue())

    def onBtnOk(self, event):

        dir = self.dirCtrl.GetValue()
        name = self.nameCtrl.GetValue()
        configfile = os.path.join(dir, 'docutils.conf')
        cfg = ConfigParser.ConfigParser()
        if os.path.isfile(configfile):
            cfg.read(configfile)
        docfactory_section = 'docfactory_project: %s' % name
            
        if not os.path.isdir(dir):
            customMsgBox(self, 'Invalid Output-Directory.', 'wakeup')
        elif name == '':
            customMsgBox(self, 'You have to enter a Project-Title.', 'wakeup')
        elif (name in self.invalid_names):
            customMsgBox(self,
                         'Invalid Project-Title. Please try another title.',
                         'wakeup')
        else:
            if self.optionsCtrl.IsChecked(0):
                self.config.options['compact_lists']      = '1'
            else:
                self.config.options['compact_lists']      = ''
            if self.optionsCtrl.IsChecked(1):
                self.config.options['embed_stylesheet']   = '1'
            else:
                self.config.options['embed_stylesheet']   = ''
            if self.optionsCtrl.IsChecked(2):
                self.config.options['footnote_backlinks'] = '1'
            else:
                self.config.options['footnote_backlinks'] = ''
            if self.optionsCtrl.IsChecked(3):
                self.config.options['generator']          = '1'
            else:
                self.config.options['generator']          = ''
            if self.optionsCtrl.IsChecked(4):
                self.config.options['source_link']        = '1'
            else:
                self.config.options['source_link']        = ''
            if self.optionsCtrl.IsChecked(5):
                self.config.options['use_latex_toc']      = '1'
            else:
                self.config.options['use_latex_toc']      = ''

            self.config.options['datestamp'] = self.datestampCtrl.GetValue()
            self.config.options['stylesheet'] = self.styCtrl.GetValue()

            stylesheetpath=os.path.abspath(os.path.join(dir,self.config.options['stylesheet']))
            if not os.path.exists(stylesheetpath):
                styles = stylesheets.stylesheets.keys()
                styles.sort()
                dlg = wxSingleChoiceDialog(self,
                                           'The stylesheet does not exist.' \
                                           '\nPlease select a style to create it' \
                                           '\nor press "Cancel" to abort.',
                                           'Create stylesheet?',
                                           styles, wxOK|wxCANCEL)
                dlg.Centre()
                if dlg.ShowModal() == wxID_OK:
                    f = open(stylesheetpath, 'wt')
                    f.write(stylesheets.stylesheets[dlg.GetStringSelection()])
                    f.close()
                dlg.Destroy()

            self.config.options['output_encoding'] = self.outencCtrl.GetValue()
           
            self.config.options['footnote_references'] = self.footrefCtrl.GetStringSelection()
            self.config.options['toc_backlinks'] = self.tocliCtrl.GetStringSelection()
            
            self.saveconf()
            self.Show(0)

    def init_ctrls(self):
        self.nameCtrl.SetValue(self.project.name)
        self.dirCtrl.SetValue(self.project.directory)
        self.styCtrl.SetValue(self.config.options['stylesheet'])
        if self.config.options['datestamp'] != None:
            ds = self.config.options['datestamp']
        else:
            ds = ''
        self.datestampCtrl.SetValue(ds)
        self.outencCtrl.SetValue(self.config.options['output_encoding'])
        self.tocliCtrl.SetStringSelection(self.config.options['toc_backlinks'])
        self.footrefCtrl.SetStringSelection(self.config.options['footnote_references'])

        if self.config.options['compact_lists'] == '1':
            self.optionsCtrl.Check(0, TRUE)
        else:
            self.optionsCtrl.Check(0, FALSE)

        if self.config.options['embed_stylesheet'] == '1':
            self.optionsCtrl.Check(1, TRUE)
        else:
            self.optionsCtrl.Check(1, FALSE)

        if self.config.options['footnote_backlinks'] == '1':
            self.optionsCtrl.Check(2, TRUE)
        else:
            self.optionsCtrl.Check(2, FALSE)

        if self.config.options['generator'] == '1':
            self.optionsCtrl.Check(3, TRUE)
        else:
            self.optionsCtrl.Check(3, FALSE)
                
        if self.config.options['source_link'] == '1':
            self.optionsCtrl.Check(4, TRUE)
        else:
            self.optionsCtrl.Check(4, FALSE)

        if self.config.options['use_latex_toc'] == '1':
            self.optionsCtrl.Check(5, TRUE)
        else:
            self.optionsCtrl.Check(5, FALSE)

    def loadconf(self):
        config = Config()
        if os.path.isfile(self.configfile):
            cfg = ConfigParser.ConfigParser()
            cfg.read(self.configfile)
            for key in config.options.keys():
                if cfg.has_option('options', key):
                    config.options[key] = cfg.get('options', key)
        return config

    def saveconf(self):
        cfg = ConfigParser.ConfigParser()
        if os.path.isfile(self.configfile):
            cfg.read(self.configfile)
        if not cfg.has_section('options'):
            cfg.add_section('options')
        for key in self.config.options.keys():
            cfg.set('options', key, self.config.options[key])
        old_section = 'docfactory_project: %s' % self.project.name
        new_section = 'docfactory_project: %s' % \
                      self.nameCtrl.GetValue()
        if old_section != new_section:
            if cfg.has_section(old_section):
                cfg.remove_section(old_section)
        f = open(self.configfile, 'wt')
        cfg.write(f)
        f.close()

class Config:
    def __init__(self):
        self.options = {}
        self.options['compact_lists']       = '1'
        self.options['datestamp']           = ''
        self.options['embed_stylesheet']    = ''
        self.options['footnote_backlinks']  = '1'
        self.options['footnote_references'] = 'superscript'
        self.options['generator']           = ''
        self.options['language_code']       = 'en'
        self.options['output_encoding']     = 'UTF-8'
        self.options['source_link']         = ''
        self.options['stylesheet']          = ''
        self.options['toc_backlinks']       = 'entry'
        self.options['use_latex_toc']       = '1'

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
        mask = wxMaskColour(bmp, wxBLUE)
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
        mask = wxMaskColour(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 10),
                           wxSize(23, 23))
        b.SetToolTipString("Add tool")
        EVT_BUTTON(self, exitID, self.on_plus_btn)
        exitID = wxNewId()
        bmp = images.getPenBitmap()
        mask = wxMaskColour(bmp, wxBLUE)
        bmp.SetMask(mask)
        b = wxBitmapButton(self, exitID, bmp, wxDLG_PNT(self, 5, 26),
                           wxSize(23, 23))
        b.SetToolTipString("Edit tool")
        EVT_BUTTON(self, exitID, self.on_edit_btn)
        exitID = wxNewId()
        bmp = images.getMinusBitmap()
        mask = wxMaskColour(bmp, wxBLUE)
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
        self.init_dir = DirBrowseButton(self, -1, wxPoint(20,70), wxSize(450,-1),
                                        labelText= 'Initial Directory:',
                                        buttonText= 'Browse')
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

