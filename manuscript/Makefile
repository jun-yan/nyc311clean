source := $(wildcard *.tex)
out := $(patsubst %.tex,%.pdf,$(source))
rev := 56a50ce
bib := $(wildcard *.bib)

.PHONY: all
all: $(out)

%.pdf: %.tex $(bib)
	@latexmk -pdf $<
#	@latexmk -c

## compare modification with the latest version checked into git
## for comparison of specific commits:
##	latexdiff-vc --git -r old_githash -r new_githash --pdf source.tex
diff:
	latexdiff-vc --git --pdf --force $(source) -r $(rev)

log:
	@git log --pretty=format:"%h by %an at %ar: %s" $(source) | head -n 15

.PNONY: clean
clean:
	rm -rf $(out)\
	  *~ .*~ .\#* .Rhistory *.aux *.bbl *.blg *.out *.log *.toc\
	  *.fff *.fdb_latexmk *.fls *.ttt *diff* *oldtmp*
