#!/usr/bin/env python

"""
:author:  Dr. Gunnar Schwant
:contact: g.schwant@gmx.de
:version: 0.1.4
"""

import sys, os, time, cPickle
from   wxPython.wx          import *
from   wxPython.html        import wxHtmlWindow
from   wxPython.lib.dialogs import wxMultipleChoiceDialog
from   wxPython.help        import *
import browser
import images
from   dialogs              import *
from   controls             import CustomStyledTextCtrl
from   controls             import CustomTreeCtrl
from   docutilsadapter      import rest2html
from   docutils.utils       import relative_path
from   urllib               import quote, unquote

#-------------------------------------------------------------------------
# global variables
#-------------------------------------------------------------------------

NAME = 'DocFactory'
DATA = os.path.join(os.path.dirname(__file__), 'docfactory.dat')

# need some IDs
[wxID_WXNEWPROJ, wxID_WXNEWREST, wxID_WXOPENFILE, 
 wxID_WXREMFILE, wxID_WXSAVEFILE, wxID_WXEXITAPP, wxID_WXPUBLRESTHTML,
 wxID_WXPROJSETTINGS, wxID_WXINSERTPATH, wxID_WXCLOSEFILE,
 wxID_WXDELETEPROJ] = map(lambda init_menubar: wxNewId(), range(11))

# Accelerator-Table for key commands
ACCEL = [(wxACCEL_NORMAL,WXK_F7,wxID_WXPUBLRESTHTML),
         (wxACCEL_CTRL,ord('N'),wxID_WXNEWREST),
         (wxACCEL_CTRL,ord('O'),wxID_WXOPENFILE),
         (wxACCEL_CTRL,ord('S'),wxID_WXSAVEFILE),
         (wxACCEL_ALT,ord('X'),wxID_WXEXITAPP)]

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
            message = time.strftime("%X", time.localtime(timeStamp)) + \
                      ": " + message
        self.tc.AppendText(message + '\n')

#-------------------------------------------------------------------------

class DocFactoryFrame(wxFrame):
    def __init__(self, projects=[]):
        wxFrame.__init__(
            self, NULL, -1, NAME, wxDefaultPosition, (800, 600),
            style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)
        self.Centre()
        self.projects = projects
        self.files = []
        self.project = None
        self.editor = None
        self.activeitem = None

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

        # File
        menu=wxMenu()
        menu.Append(wxID_WXNEWREST, '&New\tCtrl+N',
                    'Create a new reST-file and add to project')
        EVT_MENU(self, wxID_WXNEWREST, self.on_file_new)
        menu.Append(wxID_WXOPENFILE, '&Open...\tCtrl+O',
                    'Open a file')
        EVT_MENU(self, wxID_WXOPENFILE, self.on_file_open)
        menu.Append(wxID_WXCLOSEFILE, '&Close',
                    'Close file')
        EVT_MENU(self, wxID_WXCLOSEFILE, self.on_file_close)
        menu.Enable(wxID_WXCLOSEFILE, FALSE)
        menu.Append(wxID_WXSAVEFILE, '&Save\tCtrl+S', 'Save file now')
        EVT_MENU(self, wxID_WXSAVEFILE, self.on_file_save)
        menu.Enable(wxID_WXSAVEFILE, FALSE)
        menu.Append(wxID_WXREMFILE, '&Remove From Project',
                    'Remove file from project')
        EVT_MENU(self, wxID_WXREMFILE, self.on_file_remove)
        menu.Enable(wxID_WXREMFILE, FALSE)
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
        menu.Append(wxID_WXDELETEPROJ, '&Delete...',
                    'Delete one or more projects')
        EVT_MENU(self, wxID_WXDELETEPROJ, self.on_project_delete)
        menu.AppendSeparator()
        menu.Append(wxID_WXPROJSETTINGS, 'Project &Settings...',
                    'Edit project settings')
        EVT_MENU(self, wxID_WXPROJSETTINGS, self.on_project_settings)
        if len(self.projects) == 0:
            menu.Enable(wxID_WXDELETEPROJ, FALSE)
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
        menu = self.mainmenu.GetMenu(3)
        if len(self.projects) > 0 and value:
            menu.Enable(wxID_WXDELETEPROJ, value)
        menu.Enable(wxID_WXPROJSETTINGS, value)

    def activateMenuItemsFileSelected(self, value):
        menu = self.mainmenu.GetMenu(0)
        if self.project != None:
            menu.Enable(wxID_WXREMFILE, value)
        else:
            menu.Enable(wxID_WXREMFILE, FALSE)
        menu.Enable(wxID_WXCLOSEFILE, value)
        menu.Enable(wxID_WXSAVEFILE, value)
        menu = self.mainmenu.GetMenu(1)
        menu.Enable(wxID_WXINSERTPATH, value)
        menu = self.mainmenu.GetMenu(2)
        menu.Enable(wxID_WXPUBLRESTHTML, value)

    def delete_project(self, project):
        if project in self.projects:
            self.projects.remove(project)
        configfile = os.path.join(project.directory, 'docutils.conf')
        if os.path.isfile(configfile):
            cfg = ConfigParser.ConfigParser()
            cfg.read(configfile)
            section = 'docfactory_project: %s' % project.name
            if cfg.has_section(section):
                cfg.remove_section(section)
                f = open(configfile, 'wt')
                cfg.write(f)
                f.close()
        try:
            self.save_projects()
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')
        self.init_tree()

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
        self.activateMenuItemsProjectOpen(FALSE)
        self.activateMenuItemsFileSelected(FALSE)
        self.activeitem = self.root
        self.tree.SelectItem(self.activeitem)
        self.tree.Expand(self.activeitem)
        if self.editor != None:
            self.nb.SetPageText(0, 'Editor')
            self.editor.Clear()
            self.editor.Enable(0)
            self.activateMenuItemsFileSelected(FALSE)
            if self.nb.GetPageCount() > 1:
                self.nb.DeletePage(1)
        
    def project_save(self):
        try:
            configfile = os.path.join(self.project.directory, 'docutils.conf')
            section = 'docfactory_project: %s' % self.project.name
            cfg = ConfigParser.ConfigParser()
            if os.path.isfile(configfile):
                cfg.read(configfile)
            if not cfg.has_section(section):
                cfg.add_section(section)
            files = ''
            for file in self.project.files:
                files = '%s;%s' % (files, file)
            if len(files) > 1:
                files = files[1:]
            cfg.set(section, 'files', files)
            f = open(configfile, 'wt')
            cfg.write(f)
            f.close()
            for i in range(len(self.projects)):
                if self.projects[i].name == self.project.name:
                    self.projects[i] = self.project
                    break
            self.save_projects()
            self.projectdirty = false
        except:
            customMsgBox(self, '%s:\n%s\n%s' % sys.exc_info(), 'error')

    def save_projects(self):
        cfg = ConfigParser.ConfigParser()
        #print DATA
        cfg.add_section('projects')
        for project in self.projects:
            cfg.set('projects', project.name, project.directory)
        f = open(DATA, 'wt')
        cfg.write(f)
        f.close()

    def InitEditorPage(self):
        # init editor
        edID = wxNewId()
        self.editor = CustomStyledTextCtrl(self.nb, edID, self.log)
        self.editor.Clear()
        self.editor.Enable(0)
        self.nb.AddPage(self.editor, 'Editor')
        self.nb.SetSelection(0)

    def publishFileAsHTML(self, file):
        if os.path.exists(file):
            if self.project != None:
                dir = self.project.directory
            else:
                dir = os.path.dirname(file)
            wxBeginBusyCursor()
            try:
                t = time.localtime(time.time())
                st = time.strftime('%d-%b-%Y, %H:%M:%S: ', t)
                wxLogMessage('%sProcessing %s.' % (st, file))
                htmlfile = self.htmlfile(file)
                warning_lines = []
                error_lines = []
                try:
                    warning_lines, error_lines = rest2html(file,
                                                           htmlfile, dir)
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
                    htmlprv = browser.HtmlPanel(self.nb, self, self.log,
                                                htmlfile)
                    self.nb.AddPage(htmlprv, 'Preview: %s'
                                    % htmlfile_basename)
                    if warning_lines == error_lines == []:
                        self.nb.SetSelection(1)
                t = time.localtime(time.time())
                st = time.strftime('%d-%b-%Y, %H:%M:%S: ', t)
                wxLogMessage('%sFinished.' % st)
            finally:
                wxEndBusyCursor()

    def htmlfile(self, file):
        if self.project != None:
            htmlfile = os.path.join(self.project.directory,
                                    os.path.splitext(os.path.basename(file))[0] \
                                    + '.html')
        else:
            htmlfile = os.path.splitext(file)[0] + '.html'
        return htmlfile

    def Notify(self):
        t = time.localtime(time.time())
        st = time.strftime(' %d-%b-%Y   %H:%M:%S', t)
        self.SetStatusText(st, 1)

    def CheckEditorChanges(self):
        go_ahead = true
        if self.editor != None:
            if self.editor.IsModified:
                dlg=wxMessageDialog(self, 'Save changes?', NAME,
                                    wxYES_NO | wxCANCEL | wxICON_QUESTION)
                result = dlg.ShowModal()
                if result == wxID_YES:
                    file = self.tree.GetItemText(self.activeitem)
                    self.editor.SaveFile(file)
                if result == wxID_CANCEL:
                    go_ahead = false
                dlg.Destroy()
        return go_ahead

    def open_file_in_editor(self, file):
        if self.nb.GetPageCount() > 1:
            self.nb.DeletePage(1)
        self.nb.SetSelection(0)
        self.editor.LoadFile(file)
        self.nb.SetPageText(0, 'Editor: %s' %
                            os.path.basename(file))
        self.editor.Enable(1)
        self.activateMenuItemsFileSelected(TRUE)

    # --------------------------------------------------------------------
    # event handlers
    # --------------------------------------------------------------------


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
            customMsgBox(self, 'Sorry, I don\'t remember any projects.',
                         'info')

    def on_project_new(self, event):
        go_ahead = true

        other_project_names = []
        for project in self.projects:
            other_project_names.append(project.name)

        if self.CheckEditorChanges():
            project = DocProject()
            dlg = projectSettingsDlg(self, project, other_project_names)
            dlg.Centre()
            if dlg.ShowModal() == wxID_CANCEL:
                go_ahead = false
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
        go_ahead = true

        other_project_names = []
        for project in self.projects:
            if project.name != self.project.name:
                other_project_names.append(project.name)

        dlg = projectSettingsDlg(self, self.project, other_project_names)
        dlg.Centre()
        if dlg.ShowModal() == wxID_CANCEL:
            go_ahead = false
        else:
            name, directory = dlg.getValues()
        dlg.Destroy()

        if go_ahead:
            sort_tree_new = false
            if self.project.name != name:
                self.projectdirty = true
                sort_tree_new = true
                for project in self.projects:
                    if project.name == self.project.name:
                        project.name = name
                self.project.name = name
            if self.project.directory != directory:
                self.project.directory = directory
                self.projectdirty = true

        if self.projectdirty:
            self.project_save()
            if sort_tree_new:
                self.init_tree()

    def on_app_exit(self, event):
        self.Close()

    def on_file_close(self, event):
        if self.CheckEditorChanges():
            if self.nb.GetPageCount() > 1:
                self.nb.DeletePage(1)
            self.nb.SetPageText(0, 'Editor')
            self.editor.Clear()
            self.editor.Enable(0)
            self.activateMenuItemsFileSelected(FALSE)
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

        go_ahead = true
        
        if self.CheckEditorChanges():

            if self.project != None:
                dir = self.project.directory
                parent = self.tree.GetItemParent(self.activeitem)
                if parent == self.root:
                    parent = self.activeitem
            else:
                dir = ''
                parent = self.root
            
            dlg = wxFileDialog (self, 'Open file',
                                dir, '', '*.txt',
                                wxOPEN|wxFILE_MUST_EXIST)
            if dlg.ShowModal() == wxID_OK:
                file = dlg.GetPath()
                if parent == self.root:
                    if file not in self.files:
                        self.files.append(file)
                    else:
                        customMsgBox(self, '%s already in workspace.' % file,
                                     'wakeup')
                        go_ahead = false
                else:
                    if file not in self.project.files:
                        self.project.add(file)
                        self.project_save()
                    else:
                        customMsgBox(self, '%s already part of project "%s".'
                                     % (file, self.project.name),
                                     'wakeup')
                        go_ahead = false
            else:
                go_ahead = false
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
        go_ahead = true

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
            go_ahead = false
        dlg.Destroy()

        if go_ahead:
            dlg = wxTextEntryDialog(
                self, 'Enter a title for this document:', 'Title', '',
                wxOK | wxCANCEL)
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
        self.activateMenuItemsFileSelected(FALSE)
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
        file = self.tree.GetItemText(self.activeitem)
        wxLogMessage('Saving %s.' % file)
        self.editor.SaveFile(file)
        if self.nb.GetPageCount() > 1:
            self.nb.DeletePage(1)

    def on_file_html(self, event):
        item = self.activeitem
        file = self.tree.GetItemText(item)
        if self.editor.IsModified:
            wxBeginBusyCursor()
            wxLogMessage('Saving %s.' % file)
            self.editor.SaveFile(file)
            wxEndBusyCursor()
        self.publishFileAsHTML(file)

    def on_insert_path(self, event):
        item = self.activeitem
        itemimage = self.tree.GetItemImage(item)
        file = self.tree.GetItemText(item)
        if self.project != None:
            dir = self.project.directory
        else:
            dir = os.path.dirname(file)
        dlg = wxFileDialog (self, "Choose file",
                            dir, '', '*.*',
                            wxOPEN|wxFILE_MUST_EXIST)
        if dlg.ShowModal() == wxID_OK:
            target = dlg.GetPath()
            self.editor.InsertText(
                self.editor.GetCurrentPos(),
                quote(relative_path(self.htmlfile(file), target)))
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

    def on_notebook_page_changed(self, event):
        event.Skip()

    def on_tree_item_activated(self, event):
        item=event.GetItem()
        go_ahead = true

        if item != self.activeitem \
           and self.tree.GetItemImage(self.activeitem) == self.im2:
            go_ahead = self.CheckEditorChanges()

        if self.nb.GetPageCount() > 1:
            self.nb.DeletePage(1)

        if go_ahead:
            self.nb.SetSelection(0)
            if item != self.activeitem:
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
                go_ahead = false

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
                    self.activateMenuItemsProjectOpen(TRUE)
                # file is not part of project:
                else:
                    self.project = None
                    self.activateMenuItemsProjectOpen(FALSE)
                file=self.tree.GetItemText(item)
                if os.path.exists(file):
                    self.open_file_in_editor(file)
                else:
                    self.editor.Clear()
                    self.editor.Enable(0)
                    self.nb.SetPageText(0, 'Editor')
                    self.activateMenuItemsFileSelected(FALSE)
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
                self.activateMenuItemsProjectOpen(TRUE)
                self.nb.SetPageText(0, 'Editor')
                self.editor.Clear()
                self.editor.Enable(0)
                self.activateMenuItemsFileSelected(FALSE)

            # item is neither file nor project:
            else:
                self.nb.SetPageText(0, 'Editor')
                self.editor.Clear()
                self.editor.Enable(0)
                self.project = None
                self.activateMenuItemsProjectOpen(FALSE)
                self.activateMenuItemsFileSelected(FALSE)

    def on_close_window(self, event):
        go_ahead = true
        if self.projectdirty:
            dlg=wxMessageDialog(self, 'Save project?', NAME,
                                wxYES_NO | wxCANCEL | wxICON_QUESTION)
            result = dlg.ShowModal()
            if result == wxID_YES:
                self.project_save()
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
                cfg = ConfigParser.ConfigParser()
                cfg.read(DATA)
                if cfg.has_section('projects'):
                    for projectname in cfg.options('projects'):
                        project = DocProject()
                        project.name = projectname
                        project.directory = cfg.get('projects', projectname)
                        configfile = os.path.join(project.directory,
                                                  'docutils.conf')
                        if os.path.isfile(configfile):
                            cfg2=ConfigParser.ConfigParser()
                            cfg2.read(configfile)
                            section = 'docfactory_project: %s' % project.name
                            if cfg2.has_section(section):
                                if cfg2.has_option(section, 'files'):
                                    project.files = cfg2.get(section, 'files').split(';')
                                    if project.files == ['']:
                                        project.files = []
                        self.projects.append(project)
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
