# -*- coding: UTF-8 -*-

#
# dugui.py
#
# Copyright (C) 2006 Facundo Batista <facundo@taniquetil.com.ar>
#
# This file is placed under the Python 2.3 license, see
# http://www.python.org/2.3/license.html
#

import wx
import wx.html as html
import user, codecs, os
import validator
from docutils.core import publish_file


# We must integrate this with gettext() later
def _(t): return t


# Menu IDs
# File
wxID_OPEN = 101
wxID_CLOSE = 102
wxID_EXIT = 103
#Actions
wxID_PROCESS = 201
wxID_CONFIGURATION = 202
#Help
wxID_USERGUIDE = 301
wxID_ABOUT = 302

# Formats
# Key: what you'll see in the radiobox
# Value: (writer name for docutils, file extension)
FORMATS = {"HTML":  ("html",  ".html"),
           "LaTeX": ("latex", ".tex"),
           "XML":   ("xml",   ".xml"),
          }

class ControlPanel(wx.Panel):
    """Panel with the files selections grid."""

    def __init__(self, parent, vsProc):
        wx.Panel.__init__(self, parent, -1, style=wx.WANTS_CHARS)
        self.parent = parent
        self.vsProc = vsProc

        # the grid
        gbs = wx.GridBagSizer(3, 4)

        # source chooser
        # static text
        label = wx.StaticText(self, -1, _("Source file:"))
        gbs.Add(label, (0,0))
        # text entry
        id = wx.NewId()
        self.teSource = wx.TextCtrl(self, id, "", size=(200,-1), style=wx.TE_PROCESS_ENTER)
        wx.EVT_TEXT_ENTER(self.teSource, id, self.enteredSourceText)
        gbs.Add(self.teSource, (0,1), flag=wx.EXPAND)
        self.vsProc.registerEditor(self.teSource, "")
        # button
        sBtn = wx.Button(self, wx.ID_OPEN)
        wx.EVT_BUTTON(sBtn, wx.ID_OPEN, parent.onFileOpen)
        gbs.Add(sBtn, (0,2))

        # destination chooser
        # static text
        label = wx.StaticText(self, -1, _("Destination file:"))
        gbs.Add(label, (1,0))
        # text entry
        id = wx.NewId()
        self.teDestin = wx.TextCtrl(self, id, "", size=(200,-1), style=wx.TE_PROCESS_ENTER)
        wx.EVT_TEXT_ENTER(self.teDestin, id, self.enteredDestinText)
        gbs.Add(self.teDestin, (1,1), flag=wx.EXPAND)
        self.vsProc.registerEditor(self.teDestin, "")
        # button
        dBtn = wx.Button(self, wx.ID_SAVE)
        wx.EVT_BUTTON(dBtn, wx.ID_SAVE, parent.onDestChooser)
        gbs.Add(dBtn, (1,2))

        # format chooser
        id = wx.NewId()
        self.options = ["HTML", "LaTeX", "XML"]
        self.rbFormat = wx.RadioBox(self, id, _("Destination format"), wx.DefaultPosition, wx.DefaultSize, self.options, 1, style=wx.RA_SPECIFY_COLS)
        wx.EVT_RADIOBOX(self.rbFormat, id, self.rbSelection)
        gbs.Add(self.rbFormat, (0,3), (3,1))

        # process button
        id = wx.NewId()
        procBtn = wx.Button(self, id, _("Process"))
        wx.EVT_BUTTON(procBtn, id, parent.goProcess)
        vsProc.registerAction(procBtn)
        gbs.Add(procBtn, (2,4))

        self.SetSizer(gbs)
        return

    def enteredSourceText(self, event):
        self.parent.onFileOpen(None, self.teSource.GetValue())
        return

    def enteredDestinText(self, event):
        self.parent.onDestChooser(None, self.teDestin.GetValue())
        return

    def rbSelection(self, event):
        # check what was chosen
        chosen = event.GetSelection()
        format = self.options[chosen]
        
        # change the filename
        filename = self.teDestin.GetValue()
        for ext in [x[1] for x in FORMATS.values()]:
            if filename.endswith(ext):
                filename = filename[:-len(ext)]
        filename += FORMATS[format][1]
        self.teDestin.SetValue(filename)
        self.enteredDestinText(None)
        return



class TextsPanel(wx.Panel):
    """Panel with the texts fields."""

    def __init__(self, parent, vsProc):
        wx.Panel.__init__(self, parent, -1, style=wx.WANTS_CHARS)
        self.vsProc = vsProc

        box = wx.BoxSizer(wx.HORIZONTAL)

        # source text
        self.oText = wx.TextCtrl(self, -1, size=(200, 100), style=wx.TE_MULTILINE|wx.TE_RICH2|wx.TE_READONLY)
        box.Add(self.oText, 1, wx.EXPAND|wx.ALL, border=10)
        self.vsProc.registerEditor(self.oText, "")
        # put monospaced font
        font = self.oText.GetFont()
        font = wx.Font(font.GetPointSize(), wx.TELETYPE,  font.GetStyle(), font.GetWeight(), font.GetUnderlined())
        self.oText.SetFont(font)        

        # destination text
        self.dText = html.HtmlWindow(self, -1, style=wx.NO_FULL_REPAINT_ON_RESIZE)
        if "gtk2" in wx.PlatformInfo:
            self.dText.SetStandardFonts()
        box.Add(self.dText, 1, wx.EXPAND|wx.ALL, border=10)

        self.SetSizer(box)
        return

    def setSourceText(self, text):
        self.vsProc.setEditorState(self.oText, bool(text))
        self.oText.SetValue(text)
        return

    def setDestinText(self, filename):
        self.dText.LoadPage(filename)

class MenuEnabler(object):
    '''Receives all the items that must be enabled or disabled according to if the there's an open project or not.'''
    def __init__(self, menuBar, itemList):
        self.menuBar = menuBar
        self.itemList = itemList
        
    def Enable(self, newState):
        for item in self.itemList:
            self.menuBar.Enable(item, newState)
        return
            

class MyFrame(wx.Frame):
    def __init__(self, parent, ID, title):
        wx.Frame.__init__(self, parent, ID, title, pos=(-1,-1), size=wx.Size(600, 400))

        self.CreateStatusBar()
        self.previousMessage = ""
        self.statusBar(_("Welcome to Docutils"))
        menuBar = wx.MenuBar()

        # Validator to check if the project is open and enable/disable the menu
        self.vsSourceOpen = validator.ValidationSupervisor(self.statusBar, "originIsOpen") 
        enablerItemList = []
        # For Process button
        self.vsProc = validator.ValidationSupervisor(self.statusBar, "vsProcess")

        #File
        menu = wx.Menu() # 0
        menu.Append(wxID_OPEN,   _("&Open")+"\tCtrl-O",   _("Open a source file to process"))
        menu.Append(wxID_CLOSE,  _("&Close")+"\tCtrl-W",  _("Close the current source file"))
        menu.Append(wxID_EXIT,   _("&Exit")+"\tCtrl-Q",   _("Exit Docutils"))
        menuBar.Append(menu, _("&File"))
        wx.EVT_MENU(self, wxID_OPEN, self.onFileOpen)
        wx.EVT_MENU(self, wxID_CLOSE, self.onFileClose)
        wx.EVT_MENU(self, wxID_EXIT, self.onFileExit)
        enablerItemList.append(wxID_CLOSE)
        #Actions
        menu = wx.Menu() # 1
        menu.Append(wxID_PROCESS,       _("&Process"),   _("Process the origin file"))
        menu.Append(wxID_CONFIGURATION, _("&Configure"), _("Configure Distutils options"))
        menuBar.Append(menu, _("&Actions"))
        wx.EVT_MENU(self, wxID_PROCESS, self.onNotImplementedYet)
        wx.EVT_MENU(self, wxID_CONFIGURATION, self.onNotImplementedYet)
        enablerItemList.append(wxID_PROCESS)
        #Help
        menu = wx.Menu() # 2
        menu.Append(wxID_USERGUIDE, _("&User guide")+"\tF1",   _("Browse the user guide"))
        menu.Append(wxID_ABOUT,     _("&About Docutils"), _("Show information about Docutils"))
        menuBar.Append(menu, _("&Help"))
        wx.EVT_MENU(self, wxID_USERGUIDE, self.onNotImplementedYet)
        wx.EVT_MENU(self, wxID_ABOUT, self.onNotImplementedYet)

        # finish setting up the menu
        self.SetMenuBar(menuBar)

        # Enabling and disabling options
        menuEnablerOpen = MenuEnabler(menuBar, enablerItemList)
        self.vsSourceOpen.registerAction(menuEnablerOpen)
        self.vsSourceOpen.registerEditor(self, "")

        # Setup the WorkArea
        self.cp = ControlPanel(self, self.vsProc)
        self.tp = TextsPanel(self, self.vsProc)
        # box
        allBox = wx.BoxSizer(wx.VERTICAL)
        allBox.Add(self.cp, 0, wx.EXPAND | wx.ALL, border=5)
        allBox.Add(self.tp, 1, wx.EXPAND | wx.ALL, border=5)
        self.SetSizer(allBox)

        # Final go
        wx.EVT_CLOSE(self, self.onFileExit)
        self.Show(True)
        return


    def statusBar(self, message):
        if message == self.previousMessage:
            return
        self.SetStatusText(message)
        self.previousMessage = message
        return

    def onNotImplementedYet(self, event):
        self.SetStatusText(_("ERROR: Menu option not yet implemented"))
        return


    #~ def OnAbout(self, event):
        #~ dlg = wx.MessageDialog(self, "This sample program shows off\n"
                              #~ "frames, menus, statusbars, and this\n"
                              #~ "message dialog.",
                              #~ "About Me", wx.ICON_EXCLAMATION | wx.ICON_INFORMATION)
        #~ dlg.ShowModal()
        #~ dlg.Destroy()

    def onFileOpen(self, event, filename=None):
        if filename is not None:
            if not os.access(filename, os.R_OK):
                dlg = wx.MessageDialog(self, _('Such file does not exists!'), _('Error'), wx.OK | wx.ICON_ERROR)
                dlg.ShowModal()
                dlg.Destroy()
                return
        else:
            # open a user selected file
            wildcard = _("Text files (*.txt)") + "|*.txt|" + \
                       _("All files (*.*)")    + "|*.*"
            dlg = wx.FileDialog(self, _("Choose a source file to process"), user.home, wildcard=wildcard, style=wx.OPEN|wx.HIDE_READONLY)
                           
            if dlg.ShowModal() != wx.ID_OK:
                dlg.Destroy()
                self.statusBar(_("The file selection was cancelled"))
                return
    
            filename = dlg.GetPath()
            dlg.Destroy()

        # update the GUI
        self.tp.setSourceText(codecs.open(filename).read())
        self.cp.teSource.SetValue(filename)
        self.vsSourceOpen.setEditorState(self, True)
        self.vsProc.setEditorState(self.cp.teSource, True)
        self.statusBar(_("A new source file was opened"))
        return

    def onFileClose(self, event):
        # widgets
        self.tp.setSourceText("")
        self.cp.teSource.SetValue("")
        self.cp.teDestin.SetValue("")
        # validators
        self.vsProc.setEditorState(self.cp.teSource, False)
        self.vsProc.setEditorState(self.cp.teDestin, False)
        self.vsSourceOpen.setEditorState(self, False)
        return True

    def onFileExit(self, event):
        self.Destroy()

    def onDestChooser(self, event, filename=None):
        if filename is None:
            # let's ask the project filename
            wildcard = _("Text files (*.txt)") + "|*.txt|" + \
                       _("All files (*.*)")    + "|*.*"
            dlg = wx.FileDialog(self, _("Choose a file to save the result"), user.home, wildcard=wildcard, style=wx.SAVE)
            if dlg.ShowModal() != wx.ID_OK:
                dlg.Destroy()
                self.statusBar(_("The file selection was cancelled"))
                return
    
            filename = dlg.GetPath()
            dlg.Destroy()

        # append the correct extension if we need to
        format = self.cp.rbFormat.GetStringSelection()
        ext = FORMATS[format][1]
        if not filename.endswith(ext):
            filename += ext
        
        # check file permission
        if os.access(filename, os.F_OK)and not os.access(filename, os.W_OK):
            dlg = wx.MessageDialog(self, _('Such name is not valid for a writable file!'), _('Error'), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        # update the GUI
        self.cp.teDestin.SetValue(filename)
        self.vsProc.setEditorState(self.cp.teDestin, True)
        self.statusBar(_("A destination file was selected"))
        return

    def goProcess(self, event):
        # gather the data
        inp = self.cp.teSource.GetValue()
        out = self.cp.teDestin.GetValue()
        format = self.cp.rbFormat.GetStringSelection()

        # process and show
        wn = FORMATS[format][0]
        publish_file(source_path=inp, destination_path=out, writer_name=wn)
        self.tp.setDestinText(out)
        return


class MyApp(wx.App):
    def OnInit(self):
        frame = MyFrame(None, -1, "Docutils")
        frame.Show(True)
        self.SetTopWindow(frame)
        return True

if __name__ == "__main__":
    app = MyApp(0)
    app.MainLoop()

