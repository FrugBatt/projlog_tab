rapport.pdf: rapport.tex
	pdflatex rapport.tex

.PHONY: clean
clean:
	rm -f *.aux *.log rapport.pdf
