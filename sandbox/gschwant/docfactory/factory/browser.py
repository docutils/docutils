"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.3
"""

import sys, os, webbrowser

from   wxPython.wx          import *
from   wxPython.html        import *
from   wxPython.lib.dialogs import wxScrolledMessageDialog
from   dialogs              import customMsgBox

if wxPlatform == '__WXMSW__':
    #from wxPython.iewin import *
    import wx.lib.iewin as iewin


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

class IEHtmlPanelOld(wxPanel):

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

        self.ie = IEHtmlWindow(self, -1, style = wxNO_FULL_REPAINT_ON_RESIZE)

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
        self.Bind(wx.EVT_SIZE, self.OnSize)

        # Hook up the event handlers for the IE window
        self.Bind(EVT_NewWindow2, self.OnNewWindow2, self.ie)
        self.Bind(EVT_DocumentComplete, self.OnDocumentComplete, self.ie)
        self.Bind(EVT_StatusTextChange, self.OnStatusTextChange, self.ie)

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

    def OnStatusTextChange(self, evt):
        if self.frame:
            self.frame.SetStatusText(evt.Text)


class IEHtmlPanel(wx.Panel):
    def __init__(self, parent, log, frame=None, htmlfile=None):
        wx.Panel.__init__(
            self, parent, -1,
            style=wx.TAB_TRAVERSAL|wx.CLIP_CHILDREN|wx.NO_FULL_REPAINT_ON_RESIZE
            )
            
        self.log = log
        if htmlfile != None:
            #print htmlfile
            self.current = htmlfile
        else:
            self.current = "about:blank"
        self.frame = frame

        if frame:
            self.titleBase = frame.GetTitle()

        sizer = wx.BoxSizer(wx.VERTICAL)
        btnSizer = wx.BoxSizer(wx.HORIZONTAL)

        self.ie = iewin.IEHtmlWindow(self, -1, style = wx.NO_FULL_REPAINT_ON_RESIZE)


        #btn = wx.Button(self, -1, "Open", style=wx.BU_EXACTFIT)
        #self.Bind(wx.EVT_BUTTON, self.OnOpenButton, btn)
        #btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        #btn = wx.Button(self, -1, "Home", style=wx.BU_EXACTFIT)
        #self.Bind(wx.EVT_BUTTON, self.OnHomeButton, btn)
        #btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        btn = wx.Button(self, -1, "<--", style=wx.BU_EXACTFIT)
        self.Bind(wx.EVT_BUTTON, self.OnPrevPageButton, btn)
        btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        btn = wx.Button(self, -1, "-->", style=wx.BU_EXACTFIT)
        self.Bind(wx.EVT_BUTTON, self.OnNextPageButton, btn)
        btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        btn = wx.Button(self, -1, "Stop", style=wx.BU_EXACTFIT)
        self.Bind(wx.EVT_BUTTON, self.OnStopButton, btn)
        btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        #btn = wx.Button(self, -1, "Search", style=wx.BU_EXACTFIT)
        #self.Bind(wx.EVT_BUTTON, self.OnSearchPageButton, btn)
        #btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        btn = wx.Button(self, -1, "Refresh", style=wx.BU_EXACTFIT)
        self.Bind(wx.EVT_BUTTON, self.OnRefreshPageButton, btn)
        btnSizer.Add(btn, 0, wx.EXPAND|wx.ALL, 2)

        txt = wx.StaticText(self, -1, "Location:")
        btnSizer.Add(txt, 0, wx.CENTER|wx.ALL, 2)

        self.location = wx.ComboBox(
                            self, -1, "", style=wx.CB_DROPDOWN|wx.PROCESS_ENTER
                            )
        
        self.Bind(wx.EVT_COMBOBOX, self.OnLocationSelect, self.location)
        self.location.Bind(wx.EVT_KEY_UP, self.OnLocationKey)
        self.location.Bind(wx.EVT_CHAR, self.IgnoreReturn)
        btnSizer.Add(self.location, 1, wx.EXPAND|wx.ALL, 2)

        sizer.Add(btnSizer, 0, wx.EXPAND)
        sizer.Add(self.ie, 1, wx.EXPAND)

        self.ie.LoadUrl(self.current)
        self.location.Append(self.current)

        self.SetSizer(sizer)
        # Since this is a wxWindow we have to call Layout ourselves
        self.Bind(wx.EVT_SIZE, self.OnSize)

        # Hook up the event handlers for the IE window
        self.Bind(iewin.EVT_BeforeNavigate2, self.OnBeforeNavigate2, self.ie)
        self.Bind(iewin.EVT_NewWindow2, self.OnNewWindow2, self.ie)
        self.Bind(iewin.EVT_DocumentComplete, self.OnDocumentComplete, self.ie)
        ##self.Bind(iewin.EVT_ProgressChange,  self.OnProgressChange, self.ie)
        ##self.Bind(iewin.EVT_StatusTextChange, self.OnStatusTextChange, self.ie)
        ##self.Bind(iewin.EVT_TitleChange, self.OnTitleChange, self.ie)


    def ShutdownDemo(self):
        # put the frame title back
        if self.frame:
            self.frame.SetTitle(self.titleBase)


    def OnSize(self, evt):
        self.Layout()


    def OnLocationSelect(self, evt):
        url = self.location.GetStringSelection()
        #self.log.write('OnLocationSelect: %s\n' % url)
        self.ie.Navigate(url)

    def OnLocationKey(self, evt):
        if evt.KeyCode() == wx.WXK_RETURN:
            URL = self.location.GetValue()
            self.location.Append(URL)
            self.ie.Navigate(URL)
        else:
            evt.Skip()


    def IgnoreReturn(self, evt):
        if evt.GetKeyCode() != wx.WXK_RETURN:
            evt.Skip()

    def OnOpenButton(self, event):
        dlg = wx.TextEntryDialog(self, "Open Location",
                                "Enter a full URL or local path",
                                self.current, wx.OK|wx.CANCEL)
        dlg.CentreOnParent()

        if dlg.ShowModal() == wx.ID_OK:
            self.current = dlg.GetValue()
            self.ie.Navigate(self.current)

        dlg.Destroy()

    def OnHomeButton(self, event):
        self.ie.GoHome()    ## ET Phone Home!

    def OnPrevPageButton(self, event):
        self.ie.GoBack()

    def OnNextPageButton(self, event):
        self.ie.GoForward()

    def OnStopButton(self, evt):
        self.ie.Stop()

    def OnSearchPageButton(self, evt):
        self.ie.GoSearch()

    def OnRefreshPageButton(self, evt):
        self.ie.Refresh(iewin.REFRESH_COMPLETELY)


    def logEvt(self, evt):
        pst = ""
        for name in evt.paramList:
            pst += " %s:%s " % (name, repr(getattr(evt, name)))
        #print '%s: %s\n' % (evt.eventName, pst)


    def OnBeforeNavigate2(self, evt):
        self.logEvt(evt)

    def OnNewWindow2(self, evt):
        self.logEvt(evt)
        # Veto the new window.  Cancel is defined as an "out" param
        # for this event.  See iewin.py
        evt.Cancel = True   

    def OnProgressChange(self, evt):
        self.logEvt(evt)
        
    def OnDocumentComplete(self, evt):
        self.logEvt(evt)
        self.current = evt.URL
        self.location.SetValue(self.current)

    def OnTitleChange(self, evt):
        self.logEvt(evt)
        if self.frame:
            self.frame.SetTitle(self.titleBase + ' -- ' + evt.Text)

    def OnStatusTextChange(self, evt):
        self.logEvt(evt)
        if self.frame:
            self.frame.SetStatusText(evt.Text)
