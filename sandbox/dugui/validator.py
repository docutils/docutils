# -*- coding: UTF-8 -*-

#
# validator.py
#
# Copyright (C) 2003-2006 Facundo Batista <facundo@taniquetil.com.ar>
# Copyright (C) 2003-2006 Mariano Draghi <chaghi@sion.com>
#
# This file is placed under the Python 2.3 license, see
# http://www.python.org/2.3/license.html
#


class ValidationSupervisor(object):
    def __init__(self, statusBar, name):
        self._statusBar = statusBar
        self._externals = []
        self._actions = []
        self._editors = {}
        self._editMessages = {}
        self._name = name
       
    def registerAction(self, action):
        self._actions.append(action)
        if self._allOK():
            action.Enable(True)
        else:
            action.Enable(False)
        return
    
    def registerEditor(self, editor, message, initialState=None):
        # as it's a dictionary, creating and setting is the same...
        self._editMessages[editor] = message
        self.setEditorState(editor, initialState)
        return

    def setEditorState(self, editor, newState):
        self._editors[editor] = newState
        self.checkValidations()
        return

    def checkValidations(self):
#        print "check validations in vs", self._name
        # check what to do
        if self._allOK():
            state = True
        else:
            state = False
#        print "result:", state

        # just do it in all the registered actions
        for action in self._actions:
            f = getattr(action, "Enable")
            f(state)
        return

    def registerExternalValidator(self, validator, message):
        f = getattr(validator, "validate")
        self._externals.append((f, message))
        return

    def _allOK(self):
        # check editors
        for (editor, state) in self._editors.items():
#            print "checking editor", editor, state
            if state is None:
                return False
            if state == False:
                self._statusBar(self._editMessages[editor])
                return False

        # check external validators
        for (validator, message) in self._externals:
#            print "checking external", editor, state
            result = validator()
#            print "it gave:", result
            if result is None:
                return False
            if result == False:
                self._statusBar(message)
                return False

        # everything is fine
        self._statusBar("")
        return True


# All external validators must have a "validate" method.
#
# If that method returns ... the supervisor will ...:
#    True:  keep looking in the next validators, if all returns True, clears the statusBar and enables the controllers
#    False: shows the configured error message in the statusBar and disables the controllers
#    None:  does not touch the statusBar and disables the controllers

class ExistAccountValidator(object):
    def __init__(self, entriesReport):
        self.entriesReport = entriesReport 

    def validate(self):
        if self.entriesReport.cuenta is None:
            return None
        return True

class DateFromToValidator(object):
    def __init__(self, dateSelectorFrom, dateSelectorTo):
        self.dsFrom = dateSelectorFrom
        self.dsTo   = dateSelectorTo

    def validate(self):
        dateFrom = self.dsFrom.GetDate()
        dateTo = self.dsTo.GetDate()
        if not self.dsFrom.IsEnabled() and not self.dsTo.IsEnabled():
            return True
        if dateFrom is None or dateTo is None:
            return None

        return dateFrom <= dateTo
