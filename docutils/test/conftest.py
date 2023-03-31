def pytest_report_header(config):
    import os
    import pathlib
    import platform
    import sys
    import time

    # get metadata of the local `docutils` package
    docutils_root = pathlib.Path(__file__).resolve().parents[1] / 'docutils'
    namespace = {}
    exec((docutils_root/'__init__.py').read_text(encoding='utf-8'), namespace)

    return '\n'.join((
        '',
        f'Testing Docutils {namespace["__version__"]} '
        f'with Python {sys.version.split()[0]} '
        f'on {time.strftime("%Y-%m-%d at %H:%M:%S")}',
        f'OS: {platform.system()} {platform.release()} {platform.version()} '
        f'({sys.platform}, {platform.platform()})',
        f'Working directory: {os.getcwd()}',
        f'Docutils package: {docutils_root}',
        '',
    ))


# self-test
if __name__ == '__main__':
    print(pytest_report_header(None))
