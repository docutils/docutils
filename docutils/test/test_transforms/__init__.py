import os, os.path, sys

sys.path.insert(0, os.path.abspath(os.curdir))
prev = ''
while sys.path[0] != prev:
    try:
        import DocutilsTestSupport
        break
    except ImportError:
        prev = sys.path[0]
        sys.path[0] = os.path.dirname(prev)
