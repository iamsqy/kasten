# -*- mode: Makefile -*-

TEXI=kasten.texi
BASE=kasten
CSS=https://iamsqy.github.io/kasten/gnu.org.css

.PHONY: all clean html html-ns xml docbook pdf text info latex

all: html html-ns xml docbook pdf text info latex

html:
	texi2any --html --css-ref=$(CSS) $(TEXI)

html-ns:  # non-split HTML
	texi2any --html --no-split --css-ref=$(CSS) -o $(BASE).html $(TEXI)

xml:
	texi2any --xml -o $(BASE).xml $(TEXI)

docbook:
	texi2any --docbook -o $(BASE).dbk $(TEXI)

pdf:
	texi2pdf $(TEXI) --clean

text:
	texi2any --plaintext -o $(BASE).txt $(TEXI)

info:
	texi2any --info -o $(BASE).info $(TEXI)

latex:
	texi2any --latex -o $(BASE).tex $(TEXI)

clean:
	rm -f kasten.html *.xml *.dbk *.pdf *.txt *.aux *.log *.out *.toc *.cp *.fn *.ky *.pg *.tp *.vr *.cps *.fns *.kys *.pgs *.tps *.vrs *.info *.tex
	rm -rf $(BASE) *.tmp *.toc *.dvi
	rm -rf kasten_html/
