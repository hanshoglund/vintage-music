
# Run from top level

musica-vitae: musica-vitae-pdf musica-vitae-html

musica-vitae-pdf:
	markdown2pdf \
		--template ~/Library/TeX/Templates/template.tex \
		-o text/Projects/MusicaVitae/pdf/MusicaVitae.pdf \
		src/Music/Projects/MusicaVitae.lhs;
	markdown2pdf \
		--template ~/Library/TeX/Templates/template.tex \
		-o text/Projects/MusicaVitae/pdf/Instructions.pdf \
		src/Music/Projects/MusicaVitae/Instructions.markdown


musica-vitae-html:
	pandoc \
		--standalone \
		--toc \
		-o text/Projects/MusicaVitae/html/MusicaVitae.html \
		src/Music/Projects/MusicaVitae.lhs