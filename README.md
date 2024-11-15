
# nfportalutils

<!-- badges: start -->
![R-CMD-check](https://github.com/nf-osi/nfportalutils/workflows/R-CMD-check/badge.svg?branch=develop)
<!-- badges: end -->

The goal of `nfportalutils` is to provide convenience functions for project and (meta)data management in the NF-OSI data portal scope. 
Currently, `develop` branch is default so package install and docs refer to code in this branch.  

The package interops with the [Python synapse client](https://github.com/Sage-Bionetworks/synapsePythonClient) via reticulate.
You will have to set up both (see #Installation). Outside of the tested versions, there may be some issues. The tested versions are:
- Python Synapse Client == 4.3.1
- reticulate == 1.39.0

## Docs

:point_right: [Package documentation!](https://nf-osi.github.io/nfportalutils/)  

## Installation

This presumes you have already set up R with RStudio.

1. Install `reticulate` following guide at https://rstudio.github.io/reticulate/index.html#installation. 
2. Install `synapseclient==4.3.1` following https://rstudio.github.io/reticulate/articles/python_packages.html, which will use a default environment "r-reticulate". 
3. Lastly,  install `nfportalutils`. At startup, `nfportalutils` imports `synapseclient` from the default "r-reticulate". 
 - As regular users: `remotes::install_github("nf-osi/nfportalutils", build_vignettes = TRUE)` or `remotes::install_github("nf-osi/nfportalutils@some-branch", build_vignettes = TRUE)`
 - For developers, presumably working with `devtools`:
   - Clone the repo, checkout your desired development branch.
   - Make sure the package repo root is working directory, then in R run `devtools::install()`.
4. Browse some vignettes: `browseVignettes("nfportalutils")`.

## For Users

- View function reference on docs site at [Reference](https://nf-osi.github.io/nfportalutils/reference/index.html). 
- An alternative to viewing vignettes as Articles on the [docs site](https://nf-osi.github.io/nfportalutils/index.html) is to download them with pkg install and load with e.g. `vignette("annotate-nf-processed-data", package = "nfportalutils")` to view.

## For Contributors 

### General picture
- Again, default development happens in `develop`.
- But we have a side branch called `develop-synapser` where interop tries to transition to `synapser`. 
However, current coexistence makes development a bit tricky so consult first before trying new developments there.

### Contrib workflow
- Branch of `develop` and make changes
- Run `devtools::check(vignettes = FALSE)` early and often, and definitely before submitting a PR
- Make a pull request to `develop`; this will run `R-CMD-CHECK` and `pkgdown`
- Request a reviewer if both checks pass
- Reviewer requests changes or merges

### Local development tips
- Some vignettes need to be [precomputed](https://ropensci.org/blog/2019/12/08/precompute-vignettes/). 
- Again, run `devtools::check(vignettes = FALSE)` early and often.
- At minimal, address any `ERRORS` and `WARNINGS`.
- Yes, we do have a lot of `NOTES` that need to be resolved. 
- For custom indexing after adding/updating functions, edit the `_pkgdown.yml`.
- Preview the pkg docs site locally with `pkgdown::build_site()`.


## Code of Conduct
  
Please note that the nfportalutils project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
