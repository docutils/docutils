"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.3
"""

from   wxPython.wx   import *
from   wxPython.help import *
from   shutil        import copyfile
import images, os

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

        text = wxStaticText(self, -1, 'DocFactory 0.1.3', wxPoint(50, 8))
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

    def __init__(self, parent, project):
        wxDialog.__init__(self, parent, -1, title = 'Project Settings')

        wxStaticText(self, -1, 'Title', wxPoint(8, 10))
        wxStaticText(self, -1, 'Author', wxPoint(8, 42))
        wxStaticText(self, -1, 'Contact', wxPoint(8, 74))
        wxStaticText(self, -1, 'Output-Directory', wxPoint(8, 106))
        wxStaticText(self, -1, 'Stylesheet', wxPoint(8, 138))

        wxContextHelpButton(self, pos = wxPoint(370, 8),
                            size = wxSize(23, 23))
        
        exitID = wxNewId()
        self.nameCtrl = wxTextCtrl(self, exitID, project.name,
                                   pos = wxPoint(96, 8),
                                   size = wxSize(240, 21))
        self.nameCtrl.SetHelpText('Enter a title for your project here.')
        exitID = wxNewId()
        self.authorCtrl = wxTextCtrl(self, exitID, project.author,
                                     pos = wxPoint(96, 40),
                                     size = wxSize(240, 21))
        self.authorCtrl.SetHelpText('Enter your name here. It will be ' + \
                                    'used as default value for author ' + \
                                    'in new files.')
        exitID = wxNewId()
        self.contactCtrl = wxTextCtrl(self, exitID, project.contact,
                                      pos = wxPoint(96, 72),
                                      size = wxSize(240, 21))
        self.contactCtrl.SetHelpText('Enter your e-mail-address here. ' + \
                                     'It will be used as default value ' + \
                                     'for contact in new files.')
        exitID = wxNewId()
        self.dirCtrl    = wxTextCtrl(self, exitID, project.directory,
                                     pos = wxPoint(96, 104),
                                     size = wxSize(240, 21))
        self.dirCtrl.SetHelpText('This is the directory where any ' + \
                                 'HTML-files will be created.')
        exitID = wxNewId()
        self.styCtrl    = wxTextCtrl(self, exitID, project.stylesheet,
                                     pos = wxPoint(96, 136),
                                     size = wxSize(240, 21))
        self.styCtrl.SetHelpText('Specify a stylesheet in the ' + \
                                 'output-directory which will be used ' + \
                                 'by HTML-output. You can choose an ' + \
                                 'arbitrary stylesheet by pressing ' + \
                                 '"Select". (Please note that the ' + \
                                 'selected stylesheet will be copied ' + \
                                 'to the output-directory.)')
        exitID = wxNewId()
        self.btnSelDir  = wxButton(self, exitID, 'Select',
                                   pos = wxPoint(344, 104),
                                   size = wxSize(75, 23))
        EVT_BUTTON(self, exitID, self.onBtnSelDir)
        exitID = wxNewId()
        self.btnSelSty  = wxButton(self, exitID, 'Select',
                                   pos = wxPoint(344, 136),
                                   size = wxSize(75, 23), style = 0)
        EVT_BUTTON(self, exitID, self.onBtnSelSty)
        exitID = wxNewId()
        self.ok         = wxButton(self, exitID, 'OK',
                                   pos = wxPoint(136, 176),
                                   size = wxSize(75, 23))
        EVT_BUTTON(self, exitID, self.onBtnOk)
        self.cancel     = wxButton(self, wxID_CANCEL, 'Cancel',
                                   pos = wxPoint(224, 176),
                                   size = wxSize(75, 23))
        self.ok.SetDefault()
        self.Fit()

    def onBtnSelDir(self, event):
        dir = self.dirCtrl.GetValue()
        if not os.path.isdir(dir):
            dir = ''
        dlg = wxDirDialog(self, 'Choose a directory for output files', dir)
        if dlg.ShowModal() == wxID_OK:
            newdir = dlg.GetPath()
            stylesheet = self.styCtrl.GetValue()
            stylesheetpath = os.path.join(dir, stylesheet)
            if os.path.isfile(stylesheetpath):
                try:
                    copyfile(stylesheetpath, os.path.join(newdir, stylesheet))
                except:
                    customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(),
                                 'error')
            self.dirCtrl.SetValue(newdir)
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
                stylesource = dlg.GetPath()
                filename = os.path.basename(stylesource)
                styletarget = os.path.join(dir, filename)
                if stylesource != styletarget or \
                   not os.path.exists(styletarget):
                    try:
                        copyfile(stylesource, styletarget)
                    except:
                        customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(),
                                     'error')            
                self.styCtrl.SetValue(filename)
            dlg.Destroy()

    def getValues(self):
        return(self.nameCtrl.GetValue(), self.authorCtrl.GetValue(),
               self.contactCtrl.GetValue(), self.dirCtrl.GetValue(),
               self.styCtrl.GetValue())

    def onBtnOk(self, event):
        dir = self.dirCtrl.GetValue()
        stylesheet = os.path.join(dir, self.styCtrl.GetValue())
        if not os.path.isdir(dir):
           customMsgBox(self, 'Invalid Output-Directory.', 'wakeup')
        elif not os.path.isfile(stylesheet):
           customMsgBox(self, 'Invalid Stylesheet.', 'wakeup')
        else:
           self.Show(0)
          
