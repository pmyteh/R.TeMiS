.onLoad <- function(libname, pkgname) {
  data("ISO_639_2", package="ISOcodes", envir=parent.env(environment()))

  if (getOption("LNUseDateparser", default=FALSE)) {
    # Check to see if reticulate and the dateparser python library are available
    if (requireNamespace("reticulate")) {
      if (!reticulate::py_module_available("dateparser")) {
        warning("Unable to load the dateparser package from python")
        options("LNUseDateparser"=FALSE)
      }
    } else {
      warning("Unable to load the reticulate package.")
      options("LNUseDateparser"=FALSE)
    }

    if (getOption("LNUseDateparser", default=FALSE)) {
      message("tm.plugin.lexisnexis: Using Python's dateparser library")
    } else {
      warning("Cannot use dateparser library; using build-in date parsing code. ",
              "This message can be avoided either by:\n",
              "(1) (a) Ensuring that Python 3 is installed on your system;\n",
              "    (b) Installing the R 'reticulate' package; and\n",
              "    (c) Installing the Python 'dateparser' package\n",
              "or\n",
              "(2) Not setting options('LNUseDateparser' = TRUE).")
    }
  } else {
    message("tm.plugin.lexisnexis: Using built-in date parsing code.")
  }
}
