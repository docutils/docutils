all: slide-shows.html slide-shows-alt.html

slide-shows.html: slide-shows.txt
	rst2s5.py slide-shows.txt slide-shows.html --config ../../docutils.conf \
		--link-stylesheet

slide-shows-alt.html: slide-shows.txt
	rst2s5.py slide-shows.txt slide-shows-alt.html --config ../../docutils.conf \
		--theme small-black --link-stylesheet
