#!/usr/bin/env python

"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.3
"""

import sys, os, time, cPickle
from   wxPython.wx          import *
from   wxPython.html        import wxHtmlWindow
from   wxPython.lib.dialogs import wxMultipleChoiceDialog
from   wxPython.help        import *
import browser
import images
from   dialogs              import *
from   controls             import ReSTstc
from   docutilsadapter      import rest2html
from   docutils.utils       import relative_uri
from   urllib               import quote

#-------------------------------------------------------------------------
# global variables
#-------------------------------------------------------------------------

NAME = 'DocFactory'
DATA = 'dbase.dat'

# need some IDs
[wxID_WXNEWPROJ, wxID_WXOPENPROJ, wxID_WXNEWREST, wxID_WXADDFILE, wxID_WXREMFILE,
 wxID_WXSAVEFILE, wxID_WXEXITAPP, wxID_WXPUBLRESTHTML, wxID_WXPROJSETTINGS,
 wxID_WXINSERTPATH, wxID_WXCLOSEPROJ, wxID_WXDELETEPROJ] \
 = map(lambda init_menubar: wxNewId(), range(12))

# Accelerator-Table for key commands
ACCEL = [(wxACCEL_NORMAL,WXK_F7,wxID_WXPUBLRESTHTML),
         (wxACCEL_CTRL,ord('O'),wxID_WXOPENPROJ),
         (wxACCEL_CTRL,ord('N'),wxID_WXNEWREST),
         (wxACCEL_CTRL,ord('S'),wxID_WXSAVEFILE),
         (wxACCEL_ALT,ord('X'),wxID_WXEXITAPP)]

#-------------------------------------------------------------------------

class DocProject:
    def __init__(self):
        self.name = 'New project'
        self.directory = ''
        self.files = []
        self.stylesheet = ''
        self.author = ''
        self.contact = ''

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
            message = time.strftime("%X", time.localtime(timeStamp)) + \
                      ": " + message
        self.tc.AppendText(message + '\n')

#-------------------------------------------------------------------------

class DocFactoryFrame(wxFrame):
    def __init__(self, projects=[]):
        wxFrame.__init__(self, NULL, -1, NAME, wxDefaultPosition,
                         (800, 600),
                         style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)
        self.Centre()
        self.projects = projects
        self.project = None
        self.editor = None

        # Application-Icon        
        bmp = images.getLogoSmallBitmap()
        mask = wxMaskColour(bmp, wxWHITE)
        bmp.SetMask(mask)
        logoicon = wxEmptyIcon()
        logoicon.CopyFromBitmap(bmp)
        self.SetIcon(logoicon)

        self.init_menubar()
        
        self.SetAcceleratorTable(wxAcceleratorTable(ACCEL))
        
        # splitter windows
        splitter = wxSplitterWindow(self, -1, style=wxNO_3D|wxSP_3D)
        splitter2 = wxSplitterWindow(splitter, -1, style=wxNO_3D|wxSP_3D)

        # tree
        tID = wxNewId()
        self.tree = wxTreeCtrl (splitter, tID, style=wxTR_HAS_BUTTONS |
                                   wxTR_HAS_VARIABLE_ROW_HEIGHT)
        EVT_TREE_ITEM_ACTIVATED(self.tree, tID, self.on_tree_item_activated)

        # make an image list for tree
        self.im1 = self.im2 = -1
        self.il = wxImageList(16, 16)
        self.im1 = self.il.Add(images.getProjectBitmap())
        self.im2 = self.il.Add(images.getFile1Bitmap())
        self.tree.SetImageList(self.il)

        # Create a Notebook
        tID = wxNewId()
        self.nb = wxNotebook(splitter2, tID, style=wxCLIP_CHILDREN)
        EVT_NOTEBOOK_PAGE_CHANGED(self.nb, tID, self.on_notebook_page_changed)

        # Set up a log
        self.log = wxTextCtrl(splitter2, -1,
                              style = wxTE_MULTILINE|wxTE_READONLY|wxHSCROLL)

        # editor-page       
        self.InitEditorPage()

        # Set the wxWindows log target to be this textctrl
        wxLog_SetActiveTarget(CustomLog(self.log))

        # add the windows to the splitter and split it.
        splitter2.SplitHorizontally(self.nb, self.log)
        splitter2.SetSashPosition(450, true)
        splitter2.SetMinimumPaneSize(20)

        splitter.SplitVertically(self.tree, splitter2)
        splitter.SetSashPosition(180, true)
        splitter.SetMinimumPaneSize(20)

        # create a statusbar that shows the time and date on the right
        self.sb = self.CreateStatusBar(2)
        self.sb.SetStatusWidths([-1,150])
        self.statusbartimer = wxPyTimer(self.Notify)
        self.statusbartimer.Start(1000)
        self.Notify()
        
        self.Show(true)

        # Some global state variables.
        self.projectdirty = false

        EVT_CLOSE(self, self.on_close_window)

    # --------------------------------------------------------------------
    # handlers
    # --------------------------------------------------------------------

    def init_menubar(self):
        self.mainmenu = wxMenuBar()
        mainwindow = self

        # Project
        menu=wxMenu()                             
        menu.Append(wxID_WXNEWREST, '&New\tCtrl+N',
                    'Create a new reST-file and add to project')
        EVT_MENU(self, wxID_WXNEWREST, self.on_file_create)
        menu.Enable(wxID_WXNEWREST, FALSE)
        menu.Append(wxID_WXADDFILE, '&Add To Project',
                    'Add an existing file to project')
        EVT_MENU(self, wxID_WXADDFILE, self.on_file_add)
        menu.Enable(wxID_WXADDFILE, FALSE)
        menu.Append(wxID_WXREMFILE, '&Remove From Project',
                    'Remove file from project')
        EVT_MENU(self, wxID_WXREMFILE, self.on_file_remove)
        menu.Enable(wxID_WXREMFILE, FALSE)
        menu.Append(wxID_WXSAVEFILE, '&Save\tCtrl+S', 'Save file now')
        EVT_MENU(self, wxID_WXSAVEFILE, self.on_file_save)
        menu.Enable(wxID_WXSAVEFILE, FALSE)
        menu.AppendSeparator()
        menu.Append(wxID_WXEXITAPP, 'E&xit\tAlt+X', 'Exit program')
        EVT_MENU(self, wxID_WXEXITAPP, self.on_app_exit)
        self.mainmenu.Append (menu, '&File')

        # Insert
        menu=wxMenu()                             
        menu.Append(wxID_WXINSERTPATH, '&Path',
                    'Insert path')  
        EVT_MENU(self, wxID_WXINSERTPATH, self.on_insert_path)
        menu.Enable(wxID_WXINSERTPATH, FALSE)
        self.mainmenu.Append (menu, '&Insert')

        # Process
        menu=wxMenu()                             
        menu.Append(wxID_WXPUBLRESTHTML, 'To &HTML\tF7',
                    'Create HTML from active file')  
        EVT_MENU(self, wxID_WXPUBLRESTHTML, self.on_file_html)
        menu.Enable(wxID_WXPUBLRESTHTML, FALSE)
        self.mainmenu.Append (menu, '&Process')

        # Project
        menu=wxMenu()                             
        menu.Append(wxID_WXNEWPROJ, '&New', 'Create a new project')  
        EVT_MENU(self, wxID_WXNEWPROJ, self.on_project_new)    
        menu.Append(wxID_WXOPENPROJ, '&Open...\tCtrl+O',
                    'Open an existing project')
        EVT_MENU(self, wxID_WXOPENPROJ, self.on_project_open)
        menu.Append(wxID_WXCLOSEPROJ, '&Close',
                    'Close project')
        EVT_MENU(self, wxID_WXCLOSEPROJ, self.on_project_close)
        menu.Append(wxID_WXDELETEPROJ, '&Delete...',
                    'Delete one or more projects')
        EVT_MENU(self, wxID_WXDELETEPROJ, self.on_project_delete)
        menu.AppendSeparator()
        menu.Append(wxID_WXPROJSETTINGS, 'Project &Settings...',
                    'Edit project settings')
        EVT_MENU(self, wxID_WXPROJSETTINGS, self.on_project_settings)
        if len(self.projects) == 0:
            menu.Enable(wxID_WXOPENPROJ, FALSE)
            menu.Enable(wxID_WXDELETEPROJ, FALSE)
        menu.Enable(wxID_WXCLOSEPROJ, FALSE)
        menu.Enable(wxID_WXPROJSETTINGS, FALSE)
        self.mainmenu.Append (menu, 'Pr&oject')
        
        # Help
        menu=wxMenu()                             
        exitID=wxNewId()                          
        menu.Append(exitID, '&About', 'About')  
        EVT_MENU(self, exitID, self.on_help_about)
        self.mainmenu.Append (menu, '&Help')
        
        self.SetMenuBar(self.mainmenu) 

    def activateMenuItemsProjectOpen(self, value):
        menu = self.mainmenu.GetMenu(0)
        menu.Enable(wxID_WXNEWREST, value)
        menu.Enable(wxID_WXADDFILE, value)
        menu.Enable(wxID_WXREMFILE, value)
        menu.Enable(wxID_WXSAVEFILE, value)
        menu = self.mainmenu.GetMenu(3)
        if len(self.projects) > 0 and value:
            menu.Enable(wxID_WXOPENPROJ, value)
            menu.Enable(wxID_WXDELETEPROJ, value)
        menu.Enable(wxID_WXCLOSEPROJ, value)
        menu.Enable(wxID_WXPROJSETTINGS, value)
        
    def activateMenuItemsFileSelected(self, value):
        menu = self.mainmenu.GetMenu(1)
        menu.Enable(wxID_WXINSERTPATH, value)
        menu = self.mainmenu.GetMenu(2)
        menu.Enable(wxID_WXPUBLRESTHTML, value)

    def clean_up(self):
        self.tree.DeleteAllItems()
        self.activateMenuItemsProjectOpen(FALSE)
        self.activateMenuItemsFileSelected(FALSE)
        self.editor.Clear()
        self.editor.Enable(0)
        self.nb.SetPageText(0, 'Editor')
        if self.nb.GetPageCount() > 1:
            self.nb.DeletePage(1)
        self.nb.SetSelection(0)

    def delete_project(self, project):
        if project in self.projects:
            if project == self.project:
                self.clean_up()
            self.projects.remove(project)
        try:
            f = open(DATA, 'w')
            cPickle.dump(self.projects, f)
            f.close()
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def projectInit(self, project):
        try:
            if self.nb.GetPageCount() > 1:
                self.nb.DeletePage(1)
            self.project = project
            self.tree.DeleteAllItems()
            self.SetTitle('%s: %s' % (NAME, self.project.name))
            self.root = self.tree.AddRoot(self.project.name, self.im1)
            self.activeitem = self.root
            for file in self.project.files:
                self.tree.AppendItem(self.root, file, self.im2)
            self.tree.Expand(self.root)
            self.nb.SetPageText(0, 'Editor')
            self.editor.Clear()
            self.editor.Enable(0)
            self.nb.SetSelection(0)
            self.activateMenuItemsProjectOpen(TRUE)
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def projectSave(self):
        try:
            self.project.name = self.tree.GetItemText(self.root)
            for i in range(len(self.projects)):
                if self.projects[i].name == self.project.name:
                    self.projects[i] = self.project
                    break
            f = open(DATA, 'w')
            cPickle.dump(self.projects, f)
            f.close()
            self.projectdirty = false
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def InitEditorPage(self):
        # init editor
        edID = wxNewId()
        self.editor = ReSTstc(self.nb, edID, self.log)
        self.editor.Clear()
        self.editor.Enable(0)
        self.nb.AddPage(self.editor, 'Editor')
        self.nb.SetSelection(0)

    def publishFileAsHTML(self, file):
        if file != '':
            wxBeginBusyCursor()
            try:
                t = time.localtime(time.time())
                st = time.strftime('%d-%b-%Y, %H:%M:%S: ', t)
                wxLogMessage('%sProcessing %s.' % (st, file))
                htmlfile = self.htmlfile(file)
                warning_lines = []
                error_lines = []
                try:
                    warning_lines, error_lines = rest2html(file, htmlfile,
                                                           os.path.join(self.project.directory,
                                                                        self.project.stylesheet))
                finally:
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
                    self.editor.IsModified = false
                htmlfile_basename = os.path.basename(htmlfile)
                if os.path.exists(htmlfile):
                    if self.nb.GetPageCount() > 1:
                        self.nb.DeletePage(1)
                    # init html preview page
                    htmlprv = browser.HtmlPanel(self.nb, self, self.log, htmlfile)
                    self.nb.AddPage(htmlprv, 'Preview: %s' % htmlfile_basename)
                    if warning_lines == error_lines == []:
                        self.nb.SetSelection(1)
                t = time.localtime(time.time())
                st = time.strftime('%d-%b-%Y, %H:%M:%S: ', t)
                wxLogMessage('%sFinished.' % st)
            finally:
                wxEndBusyCursor()

    def htmlfile(self, file):
        htmlfile = os.path.join(self.project.directory,
                                os.path.splitext(os.path.basename(file))[0] \
                                + '.html')
        return htmlfile

    def Notify(self):
        t = time.localtime(time.time())
        st = time.strftime(' %d-%b-%Y   %H:%M:%S', t)
        self.SetStatusText(st, 1)

    def CheckEditorChanges(self):
        go_ahead = true
        if self.editor != None:
            if self.editor.IsModified:
                dlg=wxMessageDialog(self, 'Save changes?',
                                    NAME, wxYES_NO | wxCANCEL | wxICON_QUESTION)
                result = dlg.ShowModal()
                if result == wxID_YES:
                    file = self.tree.GetItemText(self.activeitem)
                    self.editor.SaveFile(file)
                if result == wxID_CANCEL:
                    go_ahead = false
                dlg.Destroy()
        return go_ahead

    # --------------------------------------------------------------------
    # event handlers
    # --------------------------------------------------------------------

    def on_project_close(self, event):
        close_it = true
        if self.projectdirty:
            dlg=wxMessageDialog(self, 'Save project?', NAME,
                                wxYES_NO | wxCANCEL | wxICON_QUESTION)
            result = dlg.ShowModal()
            if result == wxID_YES:
                self.projectSave()
            if result == wxID_CANCEL:
                close_it = false
            dlg.Destroy()
        if close_it and self.CheckEditorChanges():
            self.clean_up()

    def on_project_open(self, event):
        open_it = true
        if self.projectdirty:
            dlg=wxMessageDialog(self, 'Save project?', NAME,
                                wxYES_NO | wxCANCEL | wxICON_QUESTION)
            result = dlg.ShowModal()
            if result == wxID_YES:
                self.projectSave()
            if result == wxID_CANCEL:
                open_it = false
            dlg.Destroy()

        if open_it and self.CheckEditorChanges():
            available_projects = []
            for project in self.projects:
                available_projects.append(project.name)
            if available_projects != []:
                available_projects.sort()
                dlg = wxSingleChoiceDialog(self, 'Select a project', 'Projects',
                                           available_projects, wxOK|wxCANCEL)
                dlg.Centre()
                if dlg.ShowModal() == wxID_OK:
                    project_name = dlg.GetStringSelection()
                    for project in self.projects:
                        if project.name == project_name:
                            self.project = project
                            self.projectInit(self.project)
                dlg.Destroy()
            else:
                customMsgBox(self, 'Sorry, I don\'t remember any projects.', 'info')

    def on_project_delete(self, event):
        available_projects = []
        for project in self.projects:
            available_projects.append(project.name)
        if available_projects != []:
            available_projects.sort()
            dlg = wxMultipleChoiceDialog(self, 'Delete one or more projects',
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
            customMsgBox(self, 'Sorry, I don\'t remember any projects.', 'info')

    def on_project_new(self, event):
        go_ahead = true
        if self.projectdirty:
            dlg=wxMessageDialog(self, 'Save project?', NAME,
                                wxYES_NO | wxCANCEL | wxICON_QUESTION)
            dlg.Centre()
            result = dlg.ShowModal()
            if result == wxID_YES:
                self.projectSave()
            if result == wxID_CANCEL:
                go_ahead = false
            dlg.Destroy()

        if go_ahead and self.CheckEditorChanges():
            self.project = DocProject()
            dlg = projectSettingsDlg(self, self.project)
            dlg.Centre()
            if dlg.ShowModal() == wxID_CANCEL:
                go_ahead = false
            else:
                self.project.name, self.project.author, \
                                   self.project.contact, \
                                   self.project.directory, \
                                   self.project.stylesheet = dlg.getValues()
            dlg.Destroy()

        if go_ahead:
            other_project_names = []
            for project in self.projects:
                other_project_names.append(project.name)
            while self.project.name in other_project_names:
                self.project.name = self.project.name + '*'
            try:
                self.projects.append(self.project)
                f = open(DATA, 'w')
                cPickle.dump(self.projects, f)
                f.close()
                self.projectInit(self.project)
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        else:
            self.project = None

    def on_project_settings(self, event):
        go_ahead = true

        dlg = projectSettingsDlg(self, self.project)
        dlg.Centre()
        if dlg.ShowModal() == wxID_CANCEL:
            go_ahead = false
        else:
            name, author, contact, directory, stylesheet = dlg.getValues()
        dlg.Destroy()

        if go_ahead:
            if self.project.name != name:
                self.projectdirty = true
                other_project_names = []
                for project in self.projects:
                    if project.name != self.project.name:
                        other_project_names.append(project.name)
                while name in other_project_names:
                    name = name + '*'
                self.project.name = name
                self.tree.SetItemText(self.root, name)
            if self.project.author != author:
                self.project.author = author
                self.projectdirty = true
            if self.project.contact != contact:
                self.project.contact = contact
                self.projectdirty = true
            if self.project.stylesheet != stylesheet:
                self.project.stylesheet = stylesheet
                self.projectdirty = true
            if self.project.directory != directory:
                self.project.directory = directory
                self.projectdirty = true

        if self.projectdirty:
            try:
                f = open(DATA, 'w')
                cPickle.dump(self.projects, f)
                f.close()
                self.projectdirty = false
            except:
                customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def on_app_exit(self, event):
        self.Close()

    def on_file_add(self, event):
        if self.project <> None:
            dlg = wxFileDialog (self, "Choose a file to add",
                                self.project.directory, '', '*.txt',
                                wxOPEN|wxFILE_MUST_EXIST)
            if dlg.ShowModal() == wxID_OK:
                file = dlg.GetPath()
                if file not in self.project.files:
                    self.tree.AppendItem(self.root, file, self.im2)
                    self.tree.Expand(self.root)
                    self.project.add(file)
                    self.projectSave()
                else:
                    customMsgBox(self, 'This file is already part of your project.',
                                 'wakeup')
            dlg.Destroy()

    def on_file_create(self, event):
        if self.project <> None:
            go_ahead = true
            dlg = wxFileDialog (self, 'Create new file',
                                self.project.directory, '', '*.txt',
                                wxSAVE|wxOVERWRITE_PROMPT)
            if dlg.ShowModal() == wxID_OK:
                file = dlg.GetPath()
            else:
                go_ahead = false
            dlg.Destroy()

            if go_ahead:
                dlg = wxTextEntryDialog(self, 'Enter a title for this document:',
                                        'Title', '', wxOK | wxCANCEL)
                dlg.Centre()
                if dlg.ShowModal() == wxID_OK:
                    title = dlg.GetValue()
                else:
                    go_ahead = false
                dlg.Destroy()

            if go_ahead:                
                try:
                    f = open(file, 'w')
                    f.write(len(title)*'='+'\n')
                    f.write(title+'\n')
                    f.write(len(title)*'='+'\n')
                    if self.project.author != '':
                        f.write('\n:author:  %s' % self.project.author)
                    if self.project.contact != '':
                        f.write('\n:contact: %s' % self.project.contact)
                    t = time.localtime(time.time())
                    st = time.strftime('%Y/%m/%d %H:%M:%S', t)
                    f.write('\n:date:    $Date$' % st)
                    f.write('\n:version: 1.0\n\n')
                    f.close()
                    self.project.add(file)
                    self.tree.AppendItem(self.root, file, self.im2)
                    self.tree.Expand(self.root)
                    self.projectSave()
                except:
                    customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
            dlg.Destroy()

    def on_file_remove(self, event):
        if self.project <> None:
            item = self.tree.GetSelection()
            file = self.tree.GetItemText(item)
            if item != self.root and self.activeitem != self.root:
                self.tree.Delete(item)
                self.project.remove(file)
                self.projectSave()
                self.nb.SetPageText(0, 'Editor')
                self.editor.Clear()
                self.editor.Enable(0)
                dlg=wxMessageDialog(self, 'Delete file %s from disk?' % file,
                                    NAME, wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION)
                result = dlg.ShowModal()
                if result == wxID_YES:
                    try:
                        self.activateMenuItemsFileSelected(FALSE)
                        os.remove(file)
                    except:
                        customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
                dlg.Destroy()
            else:
                customMsgBox(self, 'No file selected.', 'wakeup')

    def on_file_save(self, event):
        if self.project <> None:
            if self.activeitem != self.root:
                file = self.tree.GetItemText(self.activeitem)
                wxLogMessage('Saving %s.' % file)
                self.editor.SaveFile(file)
                if self.nb.GetPageCount() > 1:
                    self.nb.DeletePage(1)
                
    def on_file_html(self, event):
        if self.project <> None:
            item = self.activeitem
            file = self.tree.GetItemText(item)
            if self.editor.IsModified:
                wxBeginBusyCursor()
                wxLogMessage('Saving %s.' % file)
                self.editor.SaveFile(file)
                wxEndBusyCursor()
            if item != self.root:
                self.publishFileAsHTML(file)
            else:
                customMsgBox(self, 'No file selected.', 'wakeup')

    def on_insert_path(self, event):
        if self.project <> None:
            item = self.activeitem
            if item != self.root:
                source = self.tree.GetItemText(item)
                dlg = wxFileDialog (self, "Choose file",
                                    self.project.directory, '', '*.*',
                                    wxOPEN|wxFILE_MUST_EXIST)
                if dlg.ShowModal() == wxID_OK:
                    target = dlg.GetPath()
                    self.editor.InsertText(self.editor.GetCurrentPos(),
                                           quote(relative_uri(self.htmlfile(source), target)))
                dlg.Destroy()
            else:
                customMsgBox(self, 'No file selected.', 'wakeup')


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

    def on_notebook_page_changed(self, event):
        event.Skip()

    def on_tree_item_activated(self, event):
        go_ahead = true

        if self.activeitem != self.root:
            go_ahead = self.CheckEditorChanges()
            self.tree.SetItemBold(self.activeitem, 0)
            self.tree.SetItemTextColour(self.activeitem, wxBLACK)

        if self.nb.GetPageCount() > 1:
            self.nb.DeletePage(1)

        if go_ahead:
            self.nb.SetSelection(0)
            item=event.GetItem()
            if item != self.activeitem:
                self.activeitem = item
            else:
                go_ahead = false
                
        if go_ahead:
            if item != self.root:
                self.tree.SetItemBold(item, 1)
                self.tree.SetItemTextColour(item, wxBLUE)
                file=self.tree.GetItemText(item)
                if os.path.exists(file):
                    self.editor.LoadFile(file)
                    self.nb.SetPageText(0, 'Editor: %s' %
                                        os.path.basename(file))
                    self.editor.Enable(1)
                    self.activateMenuItemsFileSelected(TRUE)
                else:
                    dlg=wxMessageDialog(self, '%s does not exist.\nRemove from project?' % file,
                                        NAME, wxYES_NO | wxICON_QUESTION)
                    if dlg.ShowModal() == wxID_YES:
                        self.tree.Delete(item)
                        self.project.remove(file)
                        self.projectSave()
                        self.editor.Clear()
                        self.editor.Enable(0)
                        #self.nb.SetPageText(0, 'Editor')
                    dlg.Destroy()
            else:
                self.nb.SetPageText(0, 'Editor')
                self.editor.Clear()
                self.editor.Enable(0)
                self.activateMenuItemsFileSelected(FALSE)

    def on_close_window(self, event):
        go_ahead = true
        if self.projectdirty:
            dlg=wxMessageDialog(self, 'Save project?', NAME,
                                wxYES_NO | wxCANCEL | wxICON_QUESTION)
            result = dlg.ShowModal()
            if result == wxID_YES:
                self.projectSave()
            if result == wxID_CANCEL:
                go_ahead = false
            dlg.Destroy()

        if go_ahead and self.CheckEditorChanges():
            self.statusbartimer.Stop()
            del self.statusbartimer
            self.Destroy()

#---------------------------------------------------------------------------

class FactoryApp(wxApp):
    def OnInit(self):
        provider = wxSimpleHelpProvider()
        wxHelpProvider_Set(provider)
        self.projects = []
        if self.init_projects():
            wxInitAllImageHandlers()
            frame = DocFactoryFrame(self.projects)
            self.SetTopWindow(frame)
            return true
        else:
            return false

    def init_projects(self):
        if os.path.exists(DATA):
            try:
                f = open(DATA, 'r')
                self.projects = cPickle.load(f)
                f.close()
            except:
                f = open('error.txt', 'w')
                f.write('%s:\n%s\n%s' % sys.exc_info())
                f.close()
                return false
        return true

#--------------------------------------------------------------------------

def main():
    try:
        factoryPath = os.path.dirname(__file__)
        os.chdir(factoryPath)
    except:
        pass
    app = FactoryApp(0)
    app.MainLoop()


#--------------------------------------------------------------------------

if __name__ == '__main__':
    main()

#--------------------------------------------------------------------------
