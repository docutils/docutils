@echo off
copy %~dp0docutils_wordml.py C:\dev\Python25\lib\site-packages\docutils\writers > nul
copy %~dp0rst2wordml.py C:\dev\Python25\scripts > nul
C:\dev\Python25\python C:\dev\Python25\scripts\rst2wordml.py %1 %2 %3 %4
