SRCS = $(wildcard original/*.Rmd)
OBJS = $(patsubst original/%, %, $(SRCS))

all: $(OBJS)

%.Rmd: original/%.Rmd
	Rscript -e "knitr::opts_chunk[['set']]('fig.path' = 'figure/$*-'); knitr::knit('original/$*.Rmd', output = '$*.Rmd')"


clean:
	rm -f $(OBJS) figure/*
