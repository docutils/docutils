
infodir ?= /usr/share/info

dir_entry = 'rst2texinfo'
dir_category = 'Documentation tools'
dir_description = 'Convert reStructuredText to Texinfo.'

all: rst2texinfo.info

rst2texinfo.info::
	python rst2texinfo.py --help > invocation.rst
	python rst2texinfo.py \
	    --texinfo-filename=rst2texinfo.info \
	    --texinfo-dir-entry=$(dir_entry) \
	    --texinfo-dir-category=$(dir_category) \
	    --texinfo-dir-description=$(dir_description) \
	    README.rst rst2texinfo.texi
	makeinfo --no-split rst2texinfo.texi

install-info: all
	cp -t $(infodir) rst2texinfo.info
	install-info --info-dir=$(infodir) rst2texinfo.info

uninstall-info: all
	-rm -f $(infodir)/rst2texinfo.info
	install-info --delete --info-dir=$(infodir) rst2texinfo.info

install::
	python setup.py install

uninstall::
	-rm -rf /usr/local/lib/python2.6/dist-packages/rst2texinfo-0.2-py2.6.egg/
	-rm -f /usr/local/bin/rst2texinfo.py

test::
	python test_texinfo.py -v

clean:
	-rm -f invocation.rst rst2texinfo.texi rst2texinfo.info
	-rm -rf build rst2texinfo.egg-info dist
	-rm -f *.pyc

.PHONY: all install-info uninstall-info test clean
