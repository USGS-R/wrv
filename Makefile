# Prepare package for release
#
# Note that 'AppendixC' and 'AppendixD' are built separately using R commands:
#   setwd("*")
#   knitr::knit2pdf("sir20165080_Appendix*.Rnw")
#   tools::compactPDF(paths="sir20165080_Appendix*.pdf", gs_quality="ebook")
#   knitr::purl("sir20165080_Appendix*.Rnw")
#

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
APPXC   := sir20165080_AppendixC

all: docs rd2tex install check clean

docs:
	R -q -e 'pkgload::load_all()';\
	R -q -e 'roxygen2::roxygenize()';\
	R -q -e 'pkgbuild::clean_dll()';\

build:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC);\

install: build
	cd ..;\
	R CMD INSTALL --build $(PKGNAME)_$(PKGVERS).tar.gz;\

check:
	cd ..;\
	R CMD check --no-build-vignettes $(PKGNAME)_$(PKGVERS).tar.gz;\

rd2tex:
	cd ..;\
	R CMD Rd2pdf --no-clean --no-preview --force $(PKGSRC);\
	cp -f .Rd2pdf*/Rd2.tex $(PKGSRC)/inst/misc;\
	$(RM) $(PKGSRC).pdf;\
	$(RM) -r .Rd2pdf*;\

vignettes:
	R -q -e 'inlmisc::BuildVignettes(gs_quality='\''ebook'\'')';\

datasets:
	cd ..;\
	R -q -e 'knitr::purl(input='\''$(PKGSRC)/vignettes/$(APPXC).Rnw'\'')';\
	Rscript $(APPXC).R
	mkdir -p $(PKGSRC)/data;\
	cp -i data/*.rda $(PKGSRC)/data;\
	$(RM) $(APPXC).R;\
	$(RM) -r data;\

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;\

.PHONY: all docs build install check rd2tex vignettes datasets clean
