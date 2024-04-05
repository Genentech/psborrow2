# For top-level vignettes
TOP_SRCS = $(wildcard original/_*.Rmd)
TOP_OBJS = $(patsubst original/_%.Rmd, %.Rmd, $(TOP_SRCS))

# For articles
ART_SRCS = $(wildcard original/articles/_*.Rmd)
ART_OBJS = $(patsubst original/articles/_%.Rmd, articles/_%.Rmd, $(ART_SRCS))

# Default rule to build all objects
build: $(TOP_OBJS) $(ART_OBJS)

# Rule to build top-level vignettes
%.Rmd: original/_%.Rmd
	Rscript -e "knitr::opts_chunk[['set']]('fig.path' = 'figure-$*-'); knitr::knit('original/_$*.Rmd', output = '$*.Rmd')"

# Rule to build articles
articles/%.Rmd: original/articles/_%.Rmd
	Rscript -e "knitr::opts_chunk[['set']]('fig.path' = 'articles/figure-$*-'); knitr::knit('original/articles/_$*.Rmd', output = 'articles/$*.Rmd')"

.clean: 
	rm -f $(OBJS)
	rm -rf figure-*

.PHONY: rebuild
rebuild: .clean build

.PHONY: install
install: 
	Rscript -e "devtools::build('../', vignettes = FALSE) |> install.packages(repos = NULL, build_vignettes = FALSE)"

.PHONY: uninstall
uninstall: 
	Rscript -e "remove.packages('psborrow2')"
	rm -rf ../../psborrow2_*.tar.gz

.PHONY: reinstall
reinstall: uninstall install