"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.4
"""

import sys, os, webbrowser

from   wxPython.wx          import *
from   wxPython.html        import *
from   wxPython.lib.dialogs import wxScrolledMessageDialog
from   dialogs              import customMsgBox

class HtmlWindow(wxHtmlWindow):
    def __init__(self, parent, id, log):
        wxHtmlWindow.__init__(self, parent, id)
        self.log = log

    def OnLinkClicked(self, linkinfo):
        self.log.WriteText('You clicked on link: %s\n' % linkinfo.GetHref())
        # Virtuals in the base class have been renamed with base_ on the front.
        self.base_OnLinkClicked(linkinfo)

class HtmlPanel(wxPanel):
    def __init__(self, parent, frame, log, htmlfile=None):
        wxPanel.__init__(self, parent, -1)
        self.log = log
        self.frame = frame
        self.cwd = os.path.split(sys.argv[0])[0]
        if not self.cwd:
            self.cwd = os.getcwd()

        self.html = HtmlWindow(self, -1, log)

        self.printer = wxHtmlEasyPrinting()

        self.box = wxBoxSizer(wxVERTICAL)
        self.box.Add(self.html, 1, wxGROW)

        subbox = wxBoxSizer(wxHORIZONTAL)

        exitID=wxNewId()
        btn = wxButton(self, exitID, "View In Browser")
        EVT_BUTTON(self, exitID, self.OnViewInBrowser)
        subbox.Add(btn, 1, wxGROW | wxALL, 2)

        exitID=wxNewId()
        btn = wxButton(self, exitID, "View Source")
        EVT_BUTTON(self, exitID, self.OnViewSource)
        subbox.Add(btn, 1, wxGROW | wxALL, 2)

        exitID=wxNewId()
        btn = wxButton(self, exitID, "Back")
        EVT_BUTTON(self, exitID, self.OnBack)
        subbox.Add(btn, 1, wxGROW | wxALL, 2)

        exitID=wxNewId()
        btn = wxButton(self, exitID, "Forward")
        EVT_BUTTON(self, exitID, self.OnForward)
        subbox.Add(btn, 1, wxGROW | wxALL, 2)

        exitID=wxNewId()
        btn = wxButton(self, exitID, "Print")
        EVT_BUTTON(self, exitID, self.OnPrint)
        subbox.Add(btn, 1, wxGROW | wxALL, 2)

        self.box.Add(subbox, 0, wxGROW)
        self.SetSizer(self.box)
        self.SetAutoLayout(true)

        self.html.LoadPage(htmlfile)

    def OnBack(self, event):
        if not self.html.HistoryBack():
            customMsgBox(self, 'No more items in history!', 'wakeup')

    def OnForward(self, event):
        if not self.html.HistoryForward():
            customMsgBox(self, 'No more items in history!', 'wakeup')

    def OnViewSource(self, event):
        source = self.html.GetParser().GetSource()
        dlg = wxScrolledMessageDialog(self, source, 'HTML Source')
        dlg.ShowModal()
        dlg.Destroy()

    def OnViewInBrowser(self, event):
        htmlfile = self.html.GetOpenedPage()
        try:
            webbrowser.open('file:%s' % htmlfile, autoraise=1)
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
            
    def OnPrint(self, event):
        self.printer.PrintFile(self.html.GetOpenedPage())

