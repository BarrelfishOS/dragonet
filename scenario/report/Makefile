###########################################################
#
# Makefile for LaTeX docs
#

DEPS=Makefile $(wildcard figures/*.pdf)

PAPER=report
LATEXOPTS=-interaction=nonstopmode
DVILATEXOPTS=-output-format=dvi -interaction=nonstopmode

all: pdf

pdf: $(PAPER).pdf

$(PAPER).xpdf: $(wildcard *.tex) $(wildcard *.bib) $(DEPS)
	pdflatex $(LATEXOPTS) $(PAPER)
	bibtex $(PAPER)
	pdflatex $(LATEXOPTS) $(PAPER)
	if [ -e $(PAPER).toc ] ; then pdflatex $(LATEXOPTS) $(PAPER) ; fi
	if [ -e $(PAPER).bbl ] ; then pdflatex $(LATEXOPTS) $(PAPER) ; fi
	if egrep Rerun $(PAPER).log ; then pdflatex $(LATEXOPTS) $(PAPER) ; fi
	if egrep Rerun $(PAPER).log ; then pdflatex $(LATEXOPTS) $(PAPER) ; fi
	if egrep Rerun $(PAPER).log ; then pdflatex $(LATEXOPTS) $(PAPER) ; fi
	gs -q -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=$(PAPER).xpdf -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -c .setpdfwrite -f $(PAPER).pdf
	$(RM) *.aux *.log *.bbl *.blg *.toc

$(PAPER).pdf: $(wildcard *.tex) $(wildcard *.bib) $(DEPS)
	pdflatex $(DVILATEXOPTS) $(PAPER)
	bibtex $(PAPER)
	pdflatex $(DVILATEXOPTS) $(PAPER)
	if [ -e $(PAPER).toc ] ; then pdflatex $(DVILATEXOPTS) $(PAPER) ; fi
	if [ -e $(PAPER).bbl ] ; then pdflatex $(DVILATEXOPTS) $(PAPER) ; fi
	if egrep Rerun $(PAPER).log ; then pdflatex $(DVILATEXOPTS) $(PAPER) ; fi
	if egrep Rerun $(PAPER).log ; then pdflatex $(DVILATEXOPTS) $(PAPER) ; fi
	if egrep Rerun $(PAPER).log ; then pdflatex $(DVILATEXOPTS) $(PAPER) ; fi
	dvips -Ppdf -Pcmz -Pamz -t letter -D 600 -G0 -o $(PAPER).ps $(PAPER).dvi
	ps2pdf14 -dPDFSETTINGS=/prepress -dEmbedAllFonts=true $(PAPER).ps $(PAPER).pdf
	$(RM) *.aux *.log *.bbl *.blg *.toc



%.ps: %.pdf
	pdf2ps $< $@

clean:
	$(RM) *.aux *.log *.bbl *.blg *~ \#*\# *.toc *.idx
	$(RM) $(patsubst %.tex, %.ps, $(wildcard *.tex))
	$(RM) $(patsubst %.tex, %.dvi, $(wildcard *.tex))
	$(RM) $(patsubst %.tex, %.pdf, $(wildcard *.tex))

