"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.2.5
"""

import sys, os, webbrowser

from   wxPython.wx          import *
from   wxPython.html        import *
from   wxPython.lib.dialogs import wxScrolledMessageDialog
from   dialogs              import customMsgBox

if wxPlatform == '__WXMSW__':
    from wxPython.iewin import *

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
        btn = wxButton(self, exitID, "<<<")
        EVT_BUTTON(self, exitID, self.OnBack)
        subbox.Add(btn, 1, wxGROW | wxALL, 2)

        exitID=wxNewId()
        btn = wxButton(self, exitID, ">>>")
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

class IEHtmlPanel(wxPanel):

    def __init__(self, parent, frame, log, htmlfile=None):
        wxPanel.__init__(self, parent, -1)
        self.log = log
        if htmlfile != None:
            self.current = htmlfile
        else:
            self.current = "about:blank"
        self.frame = frame
        if frame:
            self.titleBase = frame.GetTitle()

        sizer = wxBoxSizer(wxVERTICAL)
        btnSizer = wxBoxSizer(wxHORIZONTAL)

        self.ie = wxIEHtmlWin(self, -1, style = wxNO_FULL_REPAINT_ON_RESIZE)

        btn = wxButton(self, wxNewId(), '<<<', style=wxBU_EXACTFIT)
        EVT_BUTTON(self, btn.GetId(), self.OnPrevPageButton)
        btnSizer.Add(btn, 0, wxEXPAND|wxALL, 2)

        btn = wxButton(self, wxNewId(), '>>>', style=wxBU_EXACTFIT)
        EVT_BUTTON(self, btn.GetId(), self.OnNextPageButton)
        btnSizer.Add(btn, 0, wxEXPAND|wxALL, 2)

        btn = wxButton(self, wxNewId(), 'Stop', style=wxBU_EXACTFIT)
        EVT_BUTTON(self, btn.GetId(), self.OnStopButton)
        btnSizer.Add(btn, 0, wxEXPAND|wxALL, 2)

        btn = wxButton(self, wxNewId(), 'Refresh', style=wxBU_EXACTFIT)
        EVT_BUTTON(self, btn.GetId(), self.OnRefreshPageButton)
        btnSizer.Add(btn, 0, wxEXPAND|wxALL, 2)

        txt = wxStaticText(self, -1, 'Location:')
        btnSizer.Add(txt, 0, wxCENTER|wxALL, 2)

        self.location = wxComboBox(self, wxNewId(), '', style=wxCB_DROPDOWN|wxPROCESS_ENTER)
        EVT_COMBOBOX(self, self.location.GetId(), self.OnLocationSelect)
        EVT_KEY_UP(self.location, self.OnLocationKey)
        EVT_CHAR(self.location, self.IgnoreReturn)
        btnSizer.Add(self.location, 1, wxEXPAND|wxALL, 2)

        sizer.Add(btnSizer, 0, wxEXPAND)
        sizer.Add(self.ie, 1, wxEXPAND)

        self.ie.Navigate(self.current)
        self.location.Append(self.current)

        self.SetSizer(sizer)
        self.SetAutoLayout(true)
        EVT_SIZE(self, self.OnSize)

        # Hook up the event handlers for the IE window
        EVT_MSHTML_NEWWINDOW2(self, -1, self.OnNewWindow2)
        EVT_MSHTML_DOCUMENTCOMPLETE(self, -1, self.OnDocumentComplete)
        EVT_MSHTML_STATUSTEXTCHANGE(self, -1, self.OnStatusTextChange)

    def OnSize(self, evt):
        self.Layout()

    def OnLocationSelect(self, evt):
        url = self.location.GetStringSelection()
        self.ie.Navigate(url)

    def OnLocationKey(self, evt):
        if evt.KeyCode() == WXK_RETURN:
            URL = self.location.GetValue()
            self.location.Append(URL)
            self.ie.Navigate(URL)
        else:
            evt.Skip()

    def IgnoreReturn(self, evt):
        if evt.GetKeyCode() != WXK_RETURN:
            evt.Skip()

    def OnPrevPageButton(self, event):
        self.ie.GoBack()

    def OnNextPageButton(self, event):
        self.ie.GoForward()

    def OnStopButton(self, evt):
        self.ie.Stop()

    def OnRefreshPageButton(self, evt):
        self.ie.Refresh(wxIEHTML_REFRESH_COMPLETELY)

    def OnNewWindow2(self, evt):
        evt.Veto() # don't allow it

    def OnDocumentComplete(self, evt):
        self.current = evt.GetText1()
        self.location.SetValue(self.current)

    def OnStatusTextChange(self, evt):
        if self.frame:
            self.frame.SetStatusText(evt.GetText1())
