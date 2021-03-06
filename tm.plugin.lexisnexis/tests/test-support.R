library(tm)
library(tm.plugin.lexisnexis)

l <- tm.plugin.lexisnexis:::parsePageAndSection("Section 5; Part 2; Page 16; National Desk",
                                                "en",
                                                "test-support.R:1")
stopifnot(identical(l[["page"]], "16"))
stopifnot(identical(l[["section"]], "Section 5; Part 2; National Desk"))

l <- tm.plugin.lexisnexis:::parsePageAndSection("COMPANIES THE AMERICAS; Pg. 28",
                                                "en",
                                                "test-support.R:2")
stopifnot(identical(l[["page"]], "28"))
stopifnot(identical(l[["section"]], "COMPANIES THE AMERICAS"))

l <- tm.plugin.lexisnexis:::parsePageAndSection("IMMOBILIEN; S.20; Heft 6/2007",
                                                "de",
                                                "test-support.R:3")
stopifnot(identical(l[["page"]], "20"))
stopifnot(identical(l[["section"]], "IMMOBILIEN; Heft 6/2007"))

# Date formatting issues - dateparser
usedateparser <- getOption("LNUseDateparser", default=TRUE)
options(LNUseDateparser=TRUE)
# With 'relative date' parsing on, dateparser was parsing this as '2017,
# Semaine', and coming up with a date in 1978.
l <- tm.plugin.lexisnexis:::parseDateAndEdition("lundi 13 février 2017, Semaine Édition",
                                                "test-support.R:4",
                                                "fr")
stopifnot(l[["date"]] == as.POSIXct("2017-02-13", tz="UTC"))
stopifnot(identical(l[["edition"]], "Semaine Édition"))
options(LNUseDateparser=usedateparser)

