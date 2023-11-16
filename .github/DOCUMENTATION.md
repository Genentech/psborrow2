# Creating Documention for `psborrow2`

Because `psborrow2` relies on (`CmdStan`)[https://mc-stan.org/users/interfaces/cmdstan],
which is a shell interface to Stan missing from CRAN's system dependencies,
vignettes are pre-knitted and included in the package.

If you are making a new vignette or are proposing changes to an existing vignette,
first build the package locally (`devtools::build()`) and then 
make changes to the vignette in the `vignettes/original` directory.

Then use the Make target `build` to build the vignettes. See
`vignettes/Makefile` for other targets.