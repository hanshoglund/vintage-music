
# Run from top level

music-assure-dir:

	mkdir -p text/Music/html; \
	mkdir -p text/Music/pdf


music-pdf: music-assure-dir

	markdown2pdf \
		--table-of-contents \
		--template ~/Library/TeX/Templates/template.tex \
		-o text/Music/pdf/UserGuide.pdf \
		doc/Music.markdown

music-html: music-assure-dir

	pandoc \
		--standalone \
		--table-of-contents \
		-o text/Music/html/UserGuide.html \
		doc/Music.markdown