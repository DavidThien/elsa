.PHONY: main auto clean
main:
	latexmk -pdf -shell-escape main.tex

auto:
	latexmk -pvc -pdf -interaction=nonstopmode -shell-escape main.tex

clean:
	latexmk -C main.tex
	rm -rf main.bbl
