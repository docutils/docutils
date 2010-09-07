"""
Modeled after unix command line.

>>> z = ZipWrap("foo.zip")

Can touch files (preceeding / is optional root is either "" or "/")

>>> z.touch("/bar", "hello world\\nstuff")

Can ``cat`` files

>>> z.cat("bar")
'hello world\\nstuff'

Can ``mkdir``

>>> z.mkdir("foo/bar/baz/biz")

Can ``cat`` root directory
>>> z.cat("/")
['foo', 'bar']

>>> z.touch("foo/bar/baz/junk", "stuff")
>>> z.cat("foo/bar/baz")
['junk', 'biz']
>>> z.cat("foo/bar/baz/junk")
'stuff'
>>> z.touch("empty")
>>> z.cat("empty")
''

Can ``rm``

>>> z.rm("bar")
>>> z.cat("bar")

Can ``save``

>>> z.zipit("test/foo.zip")

Can open existing ZIP files

>>> z2 = ZipWrap("test/foo.zip")
>>> z2.cat("foo/bar/baz/junk")
'stuff'

>>> os.remove("test/foo.zip")

Can open existing directories

>>> z3 = ZipWrap("test")
>>> z3.cat("/") # root dir
['testzipwrap.py']
>>> z3.cat("/") # root dir

Note that implementation actually uses a temp directory and puts
everything there.  It's cleaned up upon garbage collection.

"""
import zipfile
import tempfile
import os
import shutil

__version__ = "0.1"
__author__ = "matt harrison"
__email__ = "matthewharrison@gmail.com"
__license__ = "psf"

class ZipWrap(object):
    def __init__(self, path):
        """
        Path can be an existing filename, or just a filename.
        """
        self.path = path
        self.src_dir = tempfile.mkdtemp()

        self.cleanup = True
        if os.path.exists(self.path):
            self._read_existing()


    def __del__(self):
        if self.cleanup:
            shutil.rmtree(self.src_dir)

    def load_zipfile(self, path):
        """
        import contents of a zipfile
        """
        #try to add as zipfile
        zin = zipfile.ZipFile(path)
        for zinfo in zin.infolist():
            name = zinfo.filename
            if name.endswith("/"):
                self.mkdir(name)
            else:
                content = zin.read(name)
                self.touch(name, content)


    def load_dir(self, path):
        """
        import contents of a directory
        """
        def visit_path(arg, dirname, names):
            for name in names:
                fpath = os.path.join(dirname, name)
                new_path = fpath[len(path):]
                if os.path.isfile(fpath):
                    content = open(fpath).read()
                    self.touch(new_path, content)
                else:
                    self.mkdir(new_path)
        os.path.walk(path, visit_path, None)


    def _read_existing(self):
        if os.path.isfile(self.path):
            #try to add as zipfile
            self.load_zipfile(self.path)
        elif os.path.isdir(self.path):
            self.load_dir(self.path)

    def cat(self, path):
        path = self._clean_path(path)
        path = os.path.join(self.src_dir, path)
        if os.path.exists(path):
            if os.path.isfile(path):
                return open(path).read()
            elif os.path.isdir(path):
                return os.listdir(path)
        else:
            raise IOError("no such file or dir %s" % path)

    def touch(self, path, contents=None):
        # make parent dirs first
        self.mkdir(os.path.dirname(path))
        path = self._clean_path(path)
        path = os.path.join(self.src_dir, path)
        fout = open(path, 'w')
        if contents:
            fout.write(contents)

    def _clean_path(self, path):
        """os.path.join acts wierd if second item starts with /"""
        if path.startswith("/"):
            path = path[1:]
        return path
        

    def mkdir(self, path):
        path = self._clean_path(path)
        full_path = os.path.join(self.src_dir, path)
        if not os.path.exists(full_path):
            os.makedirs(full_path)

    def rm(self, path):
        path = self._clean_path(path)
        path = os.path.join(self.src_dir, path)
        if os.path.exists(path):
            if os.path.isfile(path):
                os.remove(path)
            elif os.path.isdir(path):
                shutil.rmtree(path)

    def _rel_path(self, path, basepath=None):
        """
        trim off basepath
        """
        basepath = basepath or self.src_dir
        return path[len(basepath)+1:]

    def unzip(self, directory):
        """
        Write contents of zipfile to directory
        """
        if not os.path.exists(directory):
            os.makedirs(directory)
        shutil.copytree(self.src_dir, directory)

    def zipit(self, save_as=None):
        name = save_as or self.path
        zout = zipfile.ZipFile(name, 'w')

        dirs_n_files = dict(dirs=[], files=[])
        def visit_path(dnf, dirname, names):
            for name in names:
                path = os.path.join(dirname, name)

                if os.path.isfile(path):
                    dnf["files"].append(path)
                else:
                    dnf["dirs"].append(path)

        os.path.walk(self.src_dir, visit_path, dirs_n_files)
        
        # add dirs first
        # Note has issues with empty directories
        for d in dirs_n_files["dirs"]:
            new_path = d[len(self.src_dir):]           
            if not new_path.endswith("/"):
                new_path = new_path+"/"
            zinfo = zipfile.ZipInfo(new_path)
            #zinfo.external_attr = 48
            zinfo.external_attr = 16
            ##zout.writestr(zinfo, "")

        for f in dirs_n_files["files"]:
            new_path = f[len(self.src_dir):]           
            zout.write(f, new_path)


def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()
