# Implementing skips according to suggested handling when using reticulate
# See https://rstudio.github.io/reticulate/articles/package.html
# Skips tests on CRAN machines or other incompatible testing environments
# where Python can't be configured so package checks don't fail

# Skip if Python synapseclient module not installed/accessible
# This is normally imported upon package load, see `zzz.R`
skip_if_no_synapseclient <- function() {
  have_synapseclient <- py_module_available("synapseclient")
  if(!have_synapseclient)
    skip("synapseclient not available for testing")
}

# Skip if Python synapseutils module not installed/accessible
# This is normally imported upon package load, see `zzz.R`
skip_if_no_synapseutils <- function() {
  have_synapseutils <- py_module_available("synapseclient")
  if(!have_synapseutils)
    skip("synapseutils not available for testing")
}

# Skip if no pandas; pandas is needed for smaller subset of functions in the package
skip_if_no_pandas <- function() {
  have_pandas <- py_module_available("pandas")
  if(!have_pandas)
    skip("pandas not available for testing")
}

# Skip if TEST_SYNAPSE_AUTH_TOKEN not available
skip_if_no_token <- function() {
  has_token <- Sys.getenv("TEST_SYNAPSE_AUTH_TOKEN") != ""
  if(!has_token)
    skip("auth token not available for testing")
}

# If TEST_SYNAPSE_AUTH_TOKEN _is_ available but somehow unable to authenticate
# (e.g. someone pasted in wrong token), this creates a skip cascade for tests that presume
# successful login.
skip_if_no_login <- function() {
  if(!exists(".syn") || is.null(.syn$username))
    skip("not logged in for tests")
}
