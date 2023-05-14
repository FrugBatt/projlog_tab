tab:
	dune build
	mv _build/default/src/main.exe tab

all: rapport.pdf tab

rapport.pdf: rapport.tex
	pdflatex rapport.tex > /dev/null

test: tab
	./test/run_tests.sh

.PHONY: clean
clean:
	rm -f *.aux *.log rapport.pdf tab test/*.dot test/*.png
	dune clean
