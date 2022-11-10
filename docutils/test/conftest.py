def pytest_report_header(config):
    import os
    import platform
    import sys
    import time

    # DocutilsTestSupport must be imported before docutils
    from . import DocutilsTestSupport  # NoQA: F401
    import docutils

    return '\n'.join((
        '',
        f'Testing Docutils {docutils.__version__} '
        f'with Python {sys.version.split()[0]} '
        f'on {time.strftime("%Y-%m-%d at %H:%M:%S")}',
        f'OS: {platform.system()} {platform.release()} {platform.version()} '
        f'({sys.platform}, {platform.platform()})',
        f'Working directory: {os.getcwd()}',
        f'Docutils package: {os.path.dirname(docutils.__file__)}',
        '',
    ))
