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
               distribution=c("distribution"),
               # Below from Tageszeitung
                      series=c("series"))

# Regular expressions for extracting pages from a combined 
pageRxs <- list(en=";? *Pg\\. ?*(?=([;$]))",
                fr=";? *Pg\\. ?*",
                de=";? *S\\. ?*")

# Process chunked fields
split_chunk <- function(str) {
  if(length(str) > 0)
    gsub(" \n?\\([[:digit:]]{2}%)|\n", "", strsplit(str, "; ")[[1]])
  str
}

getPossibleLangs <- function() {
  handledLangs <- c("en", "fr", "de")
  
  x <- Sys.getlocale("LC_TIME")
  if (x %in% c("POSIX", "C")) x <- "en"
  l <- substr(x, 1, 2)
  
  if (l %in% handledLangs) {
    handledLangs
  } else {
    warning("Locale not set for en/fr/de. Trying to work in your ",
            "configured language as well, but this is untested.")
    c(handledLangs, l)
  }
  
}

# These generate regexes to detect day and month names, for the date parsing
# code. They should currently work in English, French, German, and your current
# configured R locale
weeknames <- paste(c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
                     "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
                     "montag", "dienstag", "mittwoch", "donnerstag", "freitag", "samstag", "sonntag",
                     weekdays(seq(as.Date("2018-01-01"), as.Date("2018-01-07"), by=1))),
                  collapse="|")
monthnames <- paste(c("january", "february", "march", "april", "may",
                      "june", "july", "august", "september", "october",
                      "november", "december",
                      "janvier", "f\u00e9vrier", "fevrier", "mars", "avril", "mai",
                      "juin", "juillet", "ao\u00fbt", "aout", "septembre",
                      "octobre", "novembre", "d\u00e9cembre", "decembre",
                      "januar", "februar", "m\u00e4rz", "marz", "april", "mai",
                      "juni", "juli", "august", "september", "oktober",
                      "november", "dezember",
                      months(seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by=31))),
                    collapse="|")

parseDateAndEdition <- function(s, tid, language=getPossibleLangs()) {
  # Parse date from date-plus-edition string.
  #
  # The core problem with LN date parsing is that the date string is localised
  # to the language of the article, while all the field codes (including the
  # LOAD-DATE:) are localised to the language of the computer operating the LN
  # website. Consequently, although we can rely on the other fields being
  # consistent within a given run, we can't rely on this.
  #
  # Further, the contents are not necessarily consistent even within a given
  # language. The following are all actual French date segments from LN:
  # * mardi 10 décembre 2019
  # * Mardi 10 Décembre 2019
  # * décembre 10 2019
  # * 10 décembre 2019 mardi 3:00 AM GMT
  #
  # The LOAD-DATE: doesn't save us except as a fallback; it can be the night
  # before actual publication, or could be later (especially if there's a
  # correction).
  
  # Even dateparser (if we're using it) is not perfect. Here we munge some known
  # issues:
  #
  # "Dienstag 10.Dezember 2019" -> "Dienstag 10. Dezember 2019"
  if(length(s) == 0 || is.na(s) || is.null(s)) {
    warning("Empty date and edition string: ", tid, ". Unable to parse.")
    return(list(date=NA, edition=character(0)))
  }
  s <- gsub('([0-9]\\.)([A-Za-z])', '\\1 \\2', s)
  
  result <- NULL
  # Try the dateparser option if it's available and we want it
  if (getOption("LNUseDateparser", default=FALSE) && requireNamespace("reticulate")) {
    result <- parseDateAndEditionDateparser(s, tid, language)
  }
  
  if (is.null(result)) result <- parseDateAndEditionClassic(s, tid, language)

  result
}

parseDateAndEditionDateparser <- function(s,
                                          tid,
                                          language=getPossibleLangs()) {
  language <- language[nchar(language) == 2]
  if (length(language) == 0) {
    ll <- NULL
  } else if (length(language) == 1) {
    ll <- list(language)
  } else {
    ll <- language
  }

  # Don't create a timezone (fairly useless given these are only days anyway);
  # don't return anything if the day/month is missing (as otherwise you're in
  # danger of getting the *current* day/month)
  dpsettings <- reticulate::dict('RETURN_AS_TIMEZONE_AWARE'=FALSE,
                                 'STRICT_PARSING'=TRUE)

  dateparser <- reticulate::import("dateparser")
  datetime <- reticulate::import("datetime")
  tup <- dateparser$search$search_dates(s, languages=ll, settings=dpsettings)[[1]]
  # Test for various kinds of misparsing
  if (!is.null(tup)) {
    parseddatestring <- tup[[1]]
    # If dateparser finds only a year, it can try to parse it erroneously.
    # 2019 can become [20]20[/]1[/]9: 9 Jan 2020. See
    # https://github.com/scrapinghub/dateparser/issues/356
    # Similarly, it is capable of picking up tiny substrings and treating
    # those as the whole date.
    
    # If the 'date' it's detected is shorter than 6 characters ('1/1/20') then
    # abandon and try the built-in classifier.
#      if (grepl('^[0-9]{4}$', tup[[1]])) tup <- NULL
    if (nchar(parseddatestring) < 6) {
      tup <- NULL
    } else {
      # There is a bug in reticulate
      # (https://github.com/rstudio/reticulate/issues/876) the conversion to
      # POSIXct fails for datetime.datetime objects without a timezone set.
      # Furthermore, the current version of reticulate coerces them all to UTC
      # anyway. And LexisNexis does not(?) return timezone information. So
      # although it's pretty grotty, just force everything to UTC here.
      dt <- tup[[2]]$replace(tzinfo=datetime$timezone$utc)
#      dt <- reticulate::py_to_r(d)
    }

  }
  # TODO: if (is.null(tup)) could try reparsing without languages restriction
  #       (slow but rare).
    
  if (is.null(tup)) {
#    warning("Could not parse document date/edition string for ",
#            tid, " using the dateparser library: ", s,
#            ". Falling back to built-in code.\n")
    # Try again without a language restriction if the first try fails
    if (!is.null(ll)) return(parseDateAndEditionDateparser(s, tid, character(0)))
    return(NULL)
  }
  
  # tup[[1]] is the part of s matched as a date.
  # \Q...\E cause the date part to be treated as literal.
  edition <- gsub(paste0('^.*\\Q', tup[[1]], '\\E'), '', s)
  edition <- cleanUpEdition(edition)
  list(date=as.POSIXct(dt), edition=edition)
}
  
parseDateAndEditionClassic <- function(s, tid, language=getPossibleLangs()) {
  # This is a bit of a guess. The first three elements are *generally*
  # month day year or day month year (with the fourth day-of-the-week), but
  # this is not always true ("Mardi 10 Décembre 2019")
  date.split <- strsplit(s, " ")[[1]]
  date.split <- date.split[date.split != ""]
  strdate <- paste(gsub(",| |\\.", "", date.split[1]),
                   gsub(",| |\\.", "", date.split[2]),
                   gsub(",| |\\.", "", date.split[3]))

  # Discard the first four substantive chunks to get edition, then clean up
  edition <- paste(tail(date.split, -4), collapse=" ")
  edition <- cleanUpEdition(edition)
#    edition <- gsub('^[[:space:]]*([^[:space:]]+[[:space:]]+){4}', '', strdate)
#    edition <- trimws(gsub("[[:space:]]+", " ", edition))

  # English uses the first format, French (generally) the second one
  s <- strptime(strdate, "%B %d %Y")
  if(is.na(s)) s <- as.POSIXct(strptime(strdate, "%d %B %Y"))
  if(is.na(s) && strdate != "") {
    # Try C locale, just in case
    old.locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    s <- strptime(strdate, "%B %d %Y")
    if(is.na(s)) s <- as.POSIXct(strptime(strdate, "%d %B %Y"))
    Sys.setlocale("LC_TIME", old.locale)

    # A bug in Mac OS gives NA when start of month name matches an abbreviated
    # name: http://www.freebsd.org/cgi/query-pr.cgi?pr=141939
    # https://stat.ethz.ch/pipermail/r-sig-mac/2012-June/009296.html
    # Add a workaround for French
    if (Sys.info()["sysname"] == "Darwin")
      s <- as.POSIXct(strptime(sub("[jJ]uillet", "07", strdate), "%d %m %Y"))

    if(is.na(s))
      warning("Could not parse document date \"", strdate, "\": ",
              tid, ". You may need to change the system locale to",
              " match that of the corpus. See LC_TIME in ",
              "?Sys.setlocale.\n")
  }
  list(date=s, edition=edition)
}

cleanUpEdition <- function(edition) {
  edition <- gsub(weeknames, "", edition)
  edition <- gsub("[[:space:]]+", " ", edition)
  edition <- gsub('^[[:punct:][:space:]]*', '', edition)
  edition <- gsub('[[:punct:][:space:]]$', '', edition)
  if (edition == '') edition <- character(0)
  edition
}

parsePageAndSection <- function(s, lang, tid) {
  l <- list(page=character(0), section=s)
  if (length(lang) == 0 || is.na(lang) || is.null(lang)) lang <- "en"
  
  if (length(s) > 0) {
    # TODO: Differentiate by language
    rx <- pageRxs[[lang]]
    
    v <- stringr::str_split(s, rx)[[1]]
    
    if (length(v) != 2) {
      # Try again with the English regex
      rx <- pageRxs[["en"]]
      v <- stringr::str_split(s, rx)[[1]]
    }
      
    if (length(v) == 2) {
      l[["page"]] <- v[[1]]
      l[["section"]] <- v[[2]]
    } else {
      message("Can't parse ", s, ": ", tid)
    }
  }
  l
}

standardiseLanguage <- function(v, tid) {
  # Take the language field given by LN and try to convert into a two-letter
  # ISO_639_2 code.
  if(length(v) > 0) {
    ISO_639_2 <- ISOcodes::ISO_639_2
    langstr <- strsplit(trimws(v), "; ")[[1]][1]
    lang <- ISO_639_2[match(tolower(langstr),
                            tolower(ISO_639_2[["Name"]])),
                      "Alpha_2"]
    # Some alpha2 codes have multiple English 'language' names: 'Dutch; Flemish'
    # is one prominent one. This will not match above. Try harder (just with
    # those languages that *have* an alpha2 code, not Middle Dutch)
    if(is.na(lang)) {
      pos <- head(grep(tolower(langstr), tolower(ISO_639_2[!is.na(ISO_639_2$Alpha_2),][["Name"]]), fixed=TRUE), n=1L)
      if (length(pos) > 0 && !is.na(pos)) lang <- ISO_639_2[!is.na(ISO_639_2$Alpha_2),]$Alpha_2[pos]
    }
    if(is.na(lang)) {
      warning("Unable to parse language for ", tid, ": ", langstr, ".\n")
      lang <- tolower(langstr)
    }
    lang
  }
  else {
    character(0)
  }
}

idFromMetadata <- function(m, pubcode, id) {
  if (length(pubcode) == 0) {
    pubcode <- gsub("[^[:alnum:]]", "", substr(m[["origin"]], 1, 10))
  }
  
  paste0(pubcode,
         strftime(m[["datetimestamp"]], format="%Y%m%d"),
         id,
         "-",
         substr(digest::digest(as.character(m),
                               algo="md5",
                               raw=FALSE),
                1, 8)
  )
  
}

wordcountFromString <- function(s) {
  if(length(s) > 0)
    as.integer(regmatches(s, regexec("[0-9]+", s))[[1]])
  else
    integer(0)
  
}
