# -*-makefile-*-
DIR_FLAGS = -D trusted -D includepath='<.>':../data -W nobackn=0
RST_FLAG_perl08 = -D xformoff='(?!Pending).......$$'
RST_FLAG_perl09 = -D perl='$$a=0;$$b=1'
RST_FLAG_perl10 = -D perl=4/0
RST_FLAG_trusted01 = -D trusted=0
