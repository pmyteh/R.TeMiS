library(tm)
library(tm.plugin.lexisnexis)

# English example
file <- system.file("texts", "lexisnexis_test_en.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))

# French example
file <- system.file("texts", "lexisnexis_test_fr.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))

# Two malformed examples: one which can be parsed but with a missing copyright
# notice (and with a warning), and one which should be dropped (with a warning).
# We suppress warnings to avoid breaking the build, then test the consequences.
file <- system.file("texts", "lexisnexis_test_copyright_error.html",
                    package = "tm.plugin.lexisnexis")
corpus <- suppressWarnings(Corpus(LexisNexisSource(file)))
stopifnot(length(corpus) == 1,
          length(corpus[[1]]$meta$rights) == 0)
if (getOption("LNUseDateparser", default=FALSE)) {
  stopifnot(corpus[[1]]$meta$id == "TheDaily201812311-83468adb")
} else {
  stopifnot(corpus[[1]]$meta$id == "TheDaily201812311-080e694f")
}

# Date formatting issues
usedateparser <- getOption("LNUseDateparser", default=FALSE)
for (o in c(TRUE, FALSE)) {
  options(LNUseDateparser=o)
  file <- system.file("texts", "lexisnexis_test_date.html",
                      package = "tm.plugin.lexisnexis")
  corpus <- Corpus(LexisNexisSource(file))
  dt <- as.POSIXlt(meta(corpus[[1]], "datetimestamp"))
  stopifnot(dt$year + 1900 == 2008,
            dt$mon + 1 == 7,
            dt$mday == 26)
  
}
options(LNUseDateparser=usedateparser)


# A set of examples of documents with multi-part main texts. That is, the main
# article text isn't contained within a single top-level <div>, but consists of
# several top-level <div>, <table>, <ul>, etc. elements.
file <- system.file("texts", "lexisnexis_test_multipart_main_texts.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))
stopifnot(length(corpus[[1]]$content) == 15)

# Documents with extraneous untagged lines that we *don't* want extracted
# as part of the main text:
file <- system.file("texts", "lexisnexis_test_extra_lines.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))
stopifnot(length(corpus[[1]]$content) == 4)


# Documents with no heading
file <- system.file("texts", "lexisnexis_test_no_headings.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))
stopifnot(all(sapply(meta(corpus, "heading"), length) == 0)) # No headings

# Documents with no body content
file <- system.file("texts", "lexisnexis_test_no_content.html",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisSource(file))
stopifnot(all(sapply(corpus, function(x) length(x$content) == 0))) # No body content

# LexisNexis Advance, .docx format, two documents.
file <- system.file("texts", "lexisnexis_advance_test_en.docx",
                    package = "tm.plugin.lexisnexis")
corpus <- Corpus(LexisNexisAdvanceSource(file))
stopifnot(corpus[[1]]$meta$id == "TimesofI201912101-5517a76f",
          corpus[[2]]$meta$id == "IIN201912102-64537e1c")

stopifnot(corpus[[1]]$meta$graphic == character(0),
          corpus[[2]]$meta$graphic == "Mi bibendum neque egestas congue quisque egestas diam.")
