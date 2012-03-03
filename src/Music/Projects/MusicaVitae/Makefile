
# Run from top level

musica-vitae: musica-vitae-pdf musica-vitae-html

musica-vitae-pdf:
	markdown2pdf \
		--template ~/Library/TeX/Templates/template.tex \
		-o text/Projects/MusicaVitae/pdf/MusicaVitae.pdf \
		src/Music/Projects/MusicaVitae.lhs

musica-vitae-html:
	pandoc \
		--standalone \
		--toc \
		-o text/Projects/MusicaVitae/html/MusicaVitae.html \
		src/Music/Projects/MusicaVitae.lhs