
infodir ?= /usr/share/info

all: rst2texinfo.info README.html

rst2texinfo.info::
	python rst2texinfo.py \
	    --texinfo-filename=rst2texinfo.info \
	    --texinfo-dir-entry='rst2texinfo' \
	    --texinfo-dir-category='Documentation tools' \
	    --texinfo-dir-description='Convert reStructuredText to Texinfo.' \
	    README.txt rst2texinfo.texi
	makeinfo --no-split rst2texinfo.texi

README.html::
	rst2html.py README.txt README.html

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
	-rm -f README.html
	-rm -f rst2texinfo.texi rst2texinfo.info
	-rm -rf build rst2texinfo.egg-info dist
	-rm -f *.pyc

.PHONY: all install-info uninstall-info test clean
