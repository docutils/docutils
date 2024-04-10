Try to install sdist from test.pypi ... and fail
================================================

shell ::

  VED=dux
  python3 -m venv $VED; cd $VED ; . bin/activate 
  (dux) engelbert@ooney:~/projects/dux$ pip list
  Package Version
  ------- -------
  pip     23.2.1

  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip

install flit from test.pypi (truncated) ::

  pip install --index-url https://test.pypi.org/simple/ flit

  Collecting flit
    Obtaining dependency information for flit from https://test-files.pythonhosted.org/
      ... flit-3.5.1-py3-none-any.whl.metadata
    Using cached https://test-files.pythonhosted.org/
      ... flit-3.5.1-py3-none-any.whl.metadata (3.6 kB)
    INFO: pip is looking at multiple versions of flit to determine which version 
      is compatible with other requirements. This could take a while.
    Obtaining dependency information for flit flit-3.2.0-py3-none-any.whl.metadata
    Using cached flit-3.2.0-py3-none-any.whl.metadata (3.6 kB)
    Obtaining dependency information for flit flit-0.11.2-py3-none-any.whl.metadata
    Using cached flit-0.11.2-py3-none-any.whl.metadata (2.5 kB)
  Collecting requests (from flit)
    Obtaining dependency information for requests 
      requests-2.5.4.1-py2.py3-none-any.whl.metadata
  Using cached ... requests-2.5.4.1-py2.py3-none-any.whl.metadata (30 kB)
  Collecting docutils (from flit)
    Obtaining dependency information for docutils 
      docutils-0.21.1-py3-none-any.whl.metadata
  Using cached https://test-files.pythonhosted.org
      docutils-0.21.1-py3-none-any.whl.metadata (2.7 kB)
  Collecting flit
    Obtaining dependency information for flit flit-0.10-py3-none-any.whl.metadata
      Using cached https://test-files. flit-0.10-py3-none-any.whl.metadata (2.8 kB)
    Obtaining dependency information for flit from flit-0.5-py3-none-any.whl.metadata
      Using cached https://test-files. flit-0.5-py3-none-any.whl.metadata (2.6 kB)
  WARNING: Package 'flit' has an invalid Requires-Python: Invalid specifier: '3'
      Using cached https://test-files. flit-0.5-py3-none-any.whl (32 kB)
      Using cached https://test-files. requests-2.5.4.1-py2.py3-none-any.whl (468 kB)

  Installing collected packages: requests, flit
  Successfully installed flit-0.5 requests-2.5.4.1

  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (dux) engelbert@ooney:~/projects/dux$ pip list
  Package  Version
  -------- -------
  flit     0.5
  pip      23.2.1
  requests 2.5.4.1

flit version 0.5 although 3.5.1 is there ?

0.5 broken "Requires-Python 3" ?

::

  (dux) pip install --upgrade pip
  (dux) pip list
  Package  Version
  -------- -------
  flit     0.5
  pip      24.0
  requests 2.5.4.1

 
no-binary all installs wheel::
 
  (duy) engelbert@ooney:~/projects/duy$ pip install --index-url https://test.pypi.org/simple/ --no-deps --no-binary all docutils==0.20.1
  Looking in indexes: https://test.pypi.org/simple/
  Collecting docutils==0.20.1
    Obtaining dependency information for docutils==0.20.1 from https://test-files.pythonhosted.org/packages/26/87/f238c0670b94533ac0353a4e2a1a771a0cc73277b88bff23d3ae35a256c1/docutils-0.20.1-py3-none-any.whl.metadata
    Downloading https://test-files.pythonhosted.org/packages/26/87/f238c0670b94533ac0353a4e2a1a771a0cc73277b88bff23d3ae35a256c1/docutils-0.20.1-py3-none-any.whl.metadata (2.8 kB)
  Using cached https://test-files.pythonhosted.org/packages/26/87/f238c0670b94533ac0353a4e2a1a771a0cc73277b88bff23d3ae35a256c1/docutils-0.20.1-py3-none-any.whl (572 kB)
  Installing collected packages: docutils
  Successfully installed docutils-0.20.1
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip list
  Package  Version
  -------- -------
  docutils 0.20.1
  pip      23.2.1
  
redo venv ::

  (duy) engelbert@ooney:~/projects/duy$ deactivate 
  engelbert@ooney:~/projects/duy$ ..
  engelbert@ooney:~/projects$ rm -r duy
  engelbert@ooney:~/projects$ VED=duy
  engelbert@ooney:~/projects$ python3 -m venv $VED; cd $VED ; . bin/activate
  (duy) engelbert@ooney:~/projects/duy$ pip install --index-url https://test.pypi.org/simple/ --no-binary docutils docutils==0.20.1
  Looking in indexes: https://test.pypi.org/simple/
  Collecting docutils==0.20.1
    Downloading https://test-files.pythonhosted.org/packages/1f/53/a5da4f2c5739cf66290fac1431ee52aff6851c7c8ffd8264f13affd7bcdd/docutils-0.20.1.tar.gz (2.1 MB)
       ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 2.1/2.1 MB 7.3 MB/s eta 0:00:00
    Installing build dependencies ... error
    error: subprocess-exited-with-error
    
    × pip subprocess to install build dependencies did not run successfully.
    │ exit code: 1
    ╰─> [6 lines of output]
        Looking in indexes: https://test.pypi.org/simple/
        ERROR: Could not find a version that satisfies the requirement setuptools>=40.8.0 (from versions: none)
        ERROR: No matching distribution found for setuptools>=40.8.0
        
        [notice] A new release of pip is available: 23.2.1 -> 24.0
        [notice] To update, run: pip install --upgrade pip
        [end of output]
    
    note: This error originates from a subprocess, and is likely not a problem with pip.
  error: subprocess-exited-with-error
  
  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> See above for output.
  
  note: This error originates from a subprocess, and is likely not a problem with pip.
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip list
  Package Version
  ------- -------
  pip     23.2.1
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip install flit_core
  Collecting flit_core
    Obtaining dependency information for flit_core from https://files.pythonhosted.org/packages/38/45/618e84e49a6c51e5dd15565ec2fcd82ab273434f236b8f108f065ded517a/flit_core-3.9.0-py3-none-any.whl.metadata
    Using cached flit_core-3.9.0-py3-none-any.whl.metadata (822 bytes)
  Using cached flit_core-3.9.0-py3-none-any.whl (63 kB)
  Installing collected packages: flit_core
  Successfully installed flit_core-3.9.0
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip list
  Package   Version
  --------- -------
  flit_core 3.9.0
  pip       23.2.1
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip install --index-url https://test.pypi.org/simple/ --no-deps --no-binary docutils docutils==0.20.1
  Looking in indexes: https://test.pypi.org/simple/
  Collecting docutils==0.20.1
    Using cached https://test-files.pythonhosted.org/packages/1f/53/a5da4f2c5739cf66290fac1431ee52aff6851c7c8ffd8264f13affd7bcdd/docutils-0.20.1.tar.gz (2.1 MB)
    Installing build dependencies ... error
    error: subprocess-exited-with-error
    
    × pip subprocess to install build dependencies did not run successfully.
    │ exit code: 1
    ╰─> [6 lines of output]
        Looking in indexes: https://test.pypi.org/simple/
        ERROR: Could not find a version that satisfies the requirement setuptools>=40.8.0 (from versions: none)
        ERROR: No matching distribution found for setuptools>=40.8.0
        
        [notice] A new release of pip is available: 23.2.1 -> 24.0
        [notice] To update, run: pip install --upgrade pip
        [end of output]
    
    note: This error originates from a subprocess, and is likely not a problem with pip.
  error: subprocess-exited-with-error
  
  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> See above for output.
  
  note: This error originates from a subprocess, and is likely not a problem with pip.
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip install setuptools
  Collecting setuptools
    Obtaining dependency information for setuptools from https://files.pythonhosted.org/packages/92/e1/1c8bb3420105e70bdf357d57dd5567202b4ef8d27f810e98bb962d950834/setuptools-69.2.0-py3-none-any.whl.metadata
    Downloading setuptools-69.2.0-py3-none-any.whl.metadata (6.3 kB)
  Downloading setuptools-69.2.0-py3-none-any.whl (821 kB)
     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 821.5/821.5 kB 7.6 MB/s eta 0:00:00
  Installing collected packages: setuptools
  Successfully installed setuptools-69.2.0
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
  (duy) engelbert@ooney:~/projects/duy$ pip install --index-url https://test.pypi.org/simple/ --no-deps --no-binary docutils docutils==0.20.1
  Looking in indexes: https://test.pypi.org/simple/
  Collecting docutils==0.20.1
    Using cached https://test-files.pythonhosted.org/packages/1f/53/a5da4f2c5739cf66290fac1431ee52aff6851c7c8ffd8264f13affd7bcdd/docutils-0.20.1.tar.gz (2.1 MB)
    Installing build dependencies ... error
    error: subprocess-exited-with-error
    
    × pip subprocess to install build dependencies did not run successfully.
    │ exit code: 1
    ╰─> [6 lines of output]
        Looking in indexes: https://test.pypi.org/simple/
        ERROR: Could not find a version that satisfies the requirement setuptools>=40.8.0 (from versions: none)
        ERROR: No matching distribution found for setuptools>=40.8.0
        
        [notice] A new release of pip is available: 23.2.1 -> 24.0
        [notice] To update, run: pip install --upgrade pip
        [end of output]
    
    note: This error originates from a subprocess, and is likely not a problem with pip.
  error: subprocess-exited-with-error
  
  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> See above for output.
  
  note: This error originates from a subprocess, and is likely not a problem with pip.
  
  [notice] A new release of pip is available: 23.2.1 -> 24.0
  [notice] To update, run: pip install --upgrade pip
 
(duy) engelbert@ooney:~/projects/duy$ pip list
Package    Version
---------- -------
flit_core  3.9.0
pip        23.2.1
setuptools 69.2.0

[notice] A new release of pip is available: 23.2.1 -> 24.0
[notice] To update, run: pip install --upgrade pip
(duy) engelbert@ooney:~/projects/duy$ pip install --index-url https://test.pypi.org/simple/ --no-deps --no-binary docutils docutils
Looking in indexes: https://test.pypi.org/simple/
Collecting docutils
  Using cached https://test-files.pythonhosted.org/packages/14/1c/642f839d386b7e88da5ed5d15ad9ae100bac9e86b4cb0781ebfebdc9c42f/docutils-0.21.1.tar.gz (2.2 MB)
  Installing build dependencies ... error
  error: subprocess-exited-with-error
  
  × pip subprocess to install build dependencies did not run successfully.
  │ exit code: 1
  ╰─> [6 lines of output]
      Looking in indexes: https://test.pypi.org/simple/
      ERROR: Could not find a version that satisfies the requirement flit_core<4,>=3.4 (from versions: none)
      ERROR: No matching distribution found for flit_core<4,>=3.4
      
      [notice] A new release of pip is available: 23.2.1 -> 24.0
      [notice] To update, run: pip install --upgrade pip
      [end of output]
  
  note: This error originates from a subprocess, and is likely not a problem with pip.
error: subprocess-exited-with-error

× pip subprocess to install build dependencies did not run successfully.
│ exit code: 1
╰─> See above for output.

note: This error originates from a subprocess, and is likely not a problem with pip.

[notice] A new release of pip is available: 23.2.1 -> 24.0
[notice] To update, run: pip install --upgrade pip
 
