# Known translations of field names
fields <- list(section=c("section", "rubrique", "rubrik"),
                      length=c("length", "longueur", "l\ue4nge"),
                      author=c("byline", "auteur", "autor"),
                      type=c("type", "publication-type", "type-publication"),
                      subject=c("subject", "sujet"),
                      language=c("language", "langue", "sprache"),
                      # The English translation is uncertain for these
                      intro=c("insert", "encart"),
                      coverage=c("geographic", "geo-localization", "localisation-geo"),
                      company=c("company", "societe"),
                      person=c("person"),
                      organisation=c("organisation", "organization"),
                      stocksymbol=c("ticker", "stock-symbol", "symbole-boursier"),
                      industry=c("industry", "activity-sector", "secteur-activite"),
                      # Some translations are uncertain for these. Like most LN field
                      # codes, they don't appear for all sources.
                      loaddate=c("load-date", "date-chargement"),
                      graphic=c("graphic"),
                      dateline=c("dateline"),
                      # This one, bizarrely, contains a space in LN Advance.
                      pubcode=c("journal-code", "journal code"),
                      # Below from the NY Times
                      doctype=c("document-type"),
                      url=c("url"),
                      highlight=c("highlight"),
                      name=c("name"),
                      correction=c("correction"),
                      correctiondate=c("correction-date"),
                      distribution=c("distribution"))

# Process chunked fields
split_chunk <- function(str) {
  if(length(str) > 0)
    gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(str, "; ")[[1]])
  str
}

# These generate regexes to detect day and month names, for the date parsing code.
# They should currently work in English, French, and your current configured R locale
weekdays <- paste(c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
                    "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
                    weekdays(seq(as.Date("2018-01-01"), as.Date("2018-01-07"), by=1))),
                  collapse="|")
months <- paste(c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december",
                  "janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin", "juillet", "ao\u00fbt", "septembre", "octobre", "novembre", "d\u00e9cembre",
                  months(seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by=31))),
                collapse="|")

parseDate <- function(s, tid) {
  # Parse date from date-plus-edition string
  date.split <- strsplit(s, " ")[[1]]
  date.split <- date.split[date.split != ""]
  strdate <- paste(gsub(",| |\\.", "", date.split[1]),
                   gsub(",| |\\.", "", date.split[2]),
                   gsub(",| |\\.", "", date.split[3]))
  # English uses the first format, French the second one
  s <- strptime(strdate, "%B %d %Y")
  if(is.na(s)) s <- strptime(strdate, "%d %B %Y")
  if(is.na(s) && strdate != "") {
    # Try C locale, just in case
    old.locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    s <- strptime(strdate, "%B %d %Y")
    if(is.na(s)) s <- strptime(strdate, "%d %B %Y")
    Sys.setlocale("LC_TIME", old.locale)

    # A bug in Mac OS gives NA when start of month name matches an abbreviated name:
    # http://www.freebsd.org/cgi/query-pr.cgi?pr=141939
    # https://stat.ethz.ch/pipermail/r-sig-mac/2012-June/009296.html
    # Add a workaround for French
    if (Sys.info()["sysname"] == "Darwin")
      s <- strptime(sub("[jJ]uillet", "07", strdate), "%d %m %Y")

    if(is.na(s))
      warning("Could not parse document date \"", strdate, "\": ",
              tid, ". You may need to change the system locale to",
              " match that of the corpus. See LC_TIME in ",
              "?Sys.setlocale.\n")
  }
  s
}

parseEdition <- function(s, tid) {
  # Parse edition from date-plus-edition string
  # Discard the first four substantive chunks to get edition, then clean up
  edition <- gsub('^[[:space:]]*([^[:space:]]+[[:space:]]+){4}', '', s)
  edition <- gsub("[[:space:]]+", " ", edition)

  edition
}


