# -*-makefile-*-
DIR_FLAGS = -W nobackn=0
RST_FLAG_if04 = -D perl='$$a=0;$$b=1'
RST_FLAG_if05 = -D perl=4/0
RST_FLAG_trusted02 = -D trusted
RST_FLAG_trusted03 = -D perl='$$a=1;open F "if07.rst"'
RST_FLAG_trusted04 = -D trusted
