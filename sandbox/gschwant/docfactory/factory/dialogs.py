"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.4
"""

from   wxPython.wx    import *
from   wxPython.help  import *
from   docutils.utils import relative_path
import images, os, ConfigParser

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
        self.SetBackgroundColour(wxColour(0, 0, 120))
        self.SetForegroundColour(wxColour(135, 138, 255))
        self.SetFont(wxFont(10, wxMODERN, wxNORMAL, wxNORMAL, false))

        bmp = images.getLogoBigBitmap()
        mask = wxMaskColour(bmp, wxWHITE)
        bmp.SetMask(mask)
        wxStaticBitmap(self, -1, bmp, wxPoint(9, 8))

        text = wxStaticText(self, -1, 'DocFactory 0.1.4', wxPoint(50, 8))
        text.SetFont(wxFont(20, wxSWISS, wxNORMAL, wxBOLD, false))

        text = wxStaticText(self , -1,
                            '>>> manufactured by:   dr. gunnar schwant',
                            wxPoint(9, 50))
        text = wxStaticText(self, -1,
                            '>>> mailto:            g.schwant@gmx.de',
                            wxPoint(9, 65))
        text = wxStaticText(self, -1,
                            '>>> Python version:    2.1.1',
                            wxPoint(9, 80))
        text = wxStaticText(self, -1,
                            '>>> wxPython version:  2.3.2.1',
                            wxPoint(9, 95))
        text = wxStaticText(self, -1,
                            '>>> Docutils version:  0.2',
                            wxPoint(9, 110))
        text = wxStaticText(self, -1,
                            '>>> special thanks to: guido van rossum,',
                            wxPoint(9, 125))
        text = wxStaticText(self, -1,
                            '...                    robin dunn',
                            wxPoint(9, 140))
        text = wxStaticText(self, -1,
                            '...                    and david goodger',
                            wxPoint(9, 155))
        text = wxStaticText(self, -1,
                            '>>> visit http://docutils.sourceforge.net',
                            wxPoint(9, 170))
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
        self.dirCtrl.SetHelpText('This is the directory where any ' + \
                                 'HTML-files will be created.')
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
                                   size = wxSize(240, 21))
        self.nameCtrl.SetHelpText('Enter a title for your project here.')

        #--------------------------------------------------
        # Docutils Page
        self.du = wxPanel(self, -1)

        # Help Button
        wxContextHelpButton(self.du, pos = wxPoint(370, 8),
                            size = wxSize(23, 23))        

        # Stylesheet
        wxStaticText(self.du, -1, 'Stylesheet', wxPoint(8, 42))
        exitID = wxNewId()
        self.styCtrl    = wxTextCtrl(self.du, exitID,
                                     pos = wxPoint(96, 40),
                                     size = wxSize(240, 21))
        self.styCtrl.SetHelpText('Path to CSS stylesheet.')
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
                      'Source-Link'
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
            dlg = wxFileDialog(self, 'Choose a stylesheet',
                               dir, '', '*.css',
                               wxOPEN|wxFILE_MUST_EXIST)
            if dlg.ShowModal() == wxID_OK:
                stylesheetpath = relative_path(self.configfile,
                                               dlg.GetPath())
                self.styCtrl.SetValue(stylesheetpath)
            dlg.Destroy()

    def getValues(self):
        return(self.adjust_str(self.nameCtrl.GetValue()), self.dirCtrl.GetValue())

    def adjust_str(self, str):
        str = str.lower().replace(' ', '_')
        for i in range(len(str)):
            if ord(str[i]) not in (range(ord('0'),ord('9')+1) + \
                                   range(ord('A'),ord('Z')+1) + \
                                   range(ord('a'),ord('z')+1)):
                str = str.replace(str[i], '_')
        return str
 
    def onBtnOk(self, event):

        dir = self.dirCtrl.GetValue()
        name = self.adjust_str(self.nameCtrl.GetValue())
        configfile = os.path.join(dir, 'docutils.conf')
        cfg = ConfigParser.ConfigParser()
        if os.path.isfile(configfile):
            cfg.read(configfile)
        docfactory_section = 'docfactory_project: %s' % name
            
        if not os.path.isdir(dir):
            customMsgBox(self, 'Invalid Output-Directory.', 'wakeup')
        elif name == '':
            customMsgBox(self, 'You have to enter a Project-Title.', 'wakeup')
        elif (name in self.invalid_names) or \
             (cfg.has_section(docfactory_section) and \
              name != self.project.name):
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

            self.config.options['datestamp'] = self.datestampCtrl.GetValue()
            self.config.options['stylesheet'] = self.styCtrl.GetValue()
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
                      self.adjust_str(self.nameCtrl.GetValue())
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
        self.options['output_encoding']     = 'UTF-8'
        self.options['source_link']         = ''
        self.options['stylesheet']          = 'default.css'
        self.options['toc_backlinks']       = 'entry'
