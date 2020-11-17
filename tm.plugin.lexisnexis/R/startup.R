.onLoad <- function(libname, pkgname) {
  data("ISO_639_2", package="ISOcodes", envir=parent.env(environment()))

  if (getOption("LNUseDateparser", default=TRUE)) {
    # Even if it was a default, it's an express choice now... unless something
    # goes wrong.
    options(LNUseDateparser=TRUE)
    # Check to see if reticulate and the dateparser python library are available
    if (requireNamespace("reticulate")) {
      if (!reticulate::py_module_available("dateparser")) {
        warning("Unable to load python's dateparser package.\n")
        options("LNUseDateparser"=FALSE)
      }
    } else {
      warning("Unable to load the reticulate R package.\n")
      options("LNUseDateparser"=FALSE)
    }

    if (!getOption("LNUseDateparser")) {
      # We *did* ask for the dateparser library (or didn't choose) but it's gone wrong.
      warning("Cannot use dateparser library; using build-in date parsing code. ",
              "This message can be avoided either by:\n",
              "(1) (a) Ensuring that Python 3 is installed on your system;\n",
              "    (b) Installing the R 'reticulate' package; and\n",
              "    (c) Installing the Python 'dateparser' package using pip\n",
              "or\n",
              "(2) Setting options('LNUseDateparser' = FALSE).")
    } else {
      packageStartupMessage("tm.plugin.lexisnexis: Using Python's dateparser library")
    }
  }
  # If we expressly chose the built-in code, or the dateparser setup failed,
  # give the appropriate notice
  if (!getOption("LNUseDateparser")) {
    packageStartupMessage("tm.plugin.lexisnexis: Using built-in date parsing code.")
  }
}
