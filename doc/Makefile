.PHONY: all clean

all: rules.pdf

rules.pdf: rules.tex
	mkdir -p build
	pdflatex --output-dir build $^
	cp build/$@ .

clean:
	rm -rf rules.pdf build
