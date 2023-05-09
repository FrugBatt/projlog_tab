rapport.pdf: rapport.tex
	pdflatex rapport.tex > /dev/null

tab:
	dune build
	mv _build/default/src/main.exe tab

all: rapport.pdf tab

test: tab
	./test/run_tests.sh

.PHONY: clean
clean:
	rm -f *.aux *.log rapport.pdf tab test/*.dot test/*.png
