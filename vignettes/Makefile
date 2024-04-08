SRCS = $(wildcard original/*.Rmd)
OBJS = $(patsubst original/_%, %, $(SRCS))

build: $(OBJS)

%.Rmd: original/_%.Rmd
	Rscript -e "knitr::opts_chunk[['set']]('fig.path' = 'figure-$*-'); knitr::knit('original/_$*.Rmd', output = '$*.Rmd')"

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