
# Run from top level

musica-vitae-pdf:
	markdown2pdf \
		--template ~/Library/TeX/Templates/template.tex \
		-o text/Projects/MusicaVitae/pdf/MusicaVitae.pdf \
		src/Music/Projects/MusicaVitae.lhs

musica-vitae-html:
	pandoc \
		--standalone \
		-o text/Projects/MusicaVitae/html/MusicaVitae.html \
		src/Music/Projects/MusicaVitae.lhs