# Known translations of field names
fields <- list(section=c("section", "rubrique", "rubrik"),
               length=c("length", "longueur", "l\ue4nge"),
               author=c("byline", "auteur", "autor"),
               type=c("type", "publication-type", "type-publication"),
               subject=c("subject", "sujet"),
               language=c("language", "langue", "sprache"),
               # The English translation is uncertain for these
               intro=c("insert", "encart"),
               coverage=c("geo-localization", "localisation-geo"),
               company=c("company", "societe"),
               stocksymbol=c("stock-symbol", "symbole-boursier"),
               industry=c("activity-sector", "secteur-activite"),
               # Some translations are uncertain for these. Like most LN field
               # codes, they don't appear for all sources.
               pubcode=c("journal-code"),
               loaddate=c("load-date", "date-chargement"),
               graphic=c("graphic"),
               dateline=c("dateline"),
               doctype=c("document-type"))

getfield <- function(nodes, field) {
    ind <- which(xml_attr(nodes, 'ln-possible-metadata') == field)
    if(length(ind) > 1) {
        warning("Multiple matches for field ", field, ": choosing the first from ", nodes[ind], "\n")
        ind <- min(ind)
    }
    if(length(ind) == 0) {
        x <- xml_children(xml_child(nodes[[ind]]))
        if(length(x) > 1) {
            # Read out the result, tag the node, return
            xml_set_attr(nodes[ind], "ln-parsed-as", field)
            xml_set_attr(nodes[ind], "ln-possible-metadata", NULL)
            return(xml_text(x[[2]]))
        }
    character(0)
}

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

parseDate <- function(s) {
    # Parse date
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
            warning(sprintf("Could not parse document date \"%s\". You may need to change the system locale to match that of the corpus. See LC_TIME in ?Sys.setlocale.", strdate))
    }
    s
}

slugToFieldname <- function(slug) {
    slug <- trimws(slug)
    # Actual field codes are a single slug of upper-case characters and hyphens,
    # followed by (just) a colon.
    if (!grepl('^[-A-Z]+:$', slug)) return(NA)
    slug <- tolower(gsub(":$", "", slug))
    for (i in seq_along(fields)) {
        if (slug %in% fields[[i]]) return(names(fields)[i])
    }
    paste0("UNKNOWN-", slug)
}

is_seq_contiguous <- function(nums) {
    x <- NA
    for (n in nums) {
        if (!is.na(x) && n != x+1) return(FALSE)
        x <- n
    }
    TRUE
}

readLexisNexisHTML <- FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        # The content of a LexisNexis HTML file are language-dependent,
        # inconsistent, only partially semantically tagged, and don't follow a
        # hierarchical format which is easy for parsing.
        #
        # The basic strategy of this function is as follows:
        # 1) Parse the HTML text with xml2
        # 2) Chunk the parsed content into content nodes
        # 3) Extract metadata using fixed position fields and by detecting LN's
        #    slugs such as "LOAD-DATE:" in consistent places at the start of lines
        # 4) Use heuristics to guess the heading, the body, the date, and the
        #    copyright notice (which are not tagged)
        # 5) Tidy up the variables where needed
        # 6) Make a PlainTextDocument and return it
        #
        # We keep track of which nodes have been processed, and what we got from
        # them, by adding attributes to nodes directly.

        # Temporary id, until we've parsed the date etc.
        id <- paste0(basename(elem$uri), ":", id)

        #####
        # 1: Parsing
        #####
        # textConnection() in LexisNexisSource() converts strings to UTF-8
        tree <- read_html(elem$content, asText=TRUE, encoding="UTF-8")
        # Strip unhelpful <style> and <br> elements
        xml_remove(xml_find_all(tree, "//style"), free=TRUE)
        xml_remove(xml_find_all(tree, "//br"), free=TRUE)

        #####
        # 2: Chunking
        #####

        nodes <- xml_find_all(tree, "body/*")

        # Trim empty and spurious nodes
        nodes <- nodes[xml_text(nodes) != ""]
        if(xml_text(nodes[[1]], trim=TRUE) == "Return to List")
            nodes <- nodes[-1]

        # Extract first field on each line, which contains metadata entry type
        # We use this for doing lookups on known metadata fields
        node_leads <- xml_text(xml_find_first(nodes, '*[1]/*'), trim=TRUE)
        for (i in 1:length(node_leads)) {
            f <- slugToFieldname(node_leads[i])
            if (!is.na(f)) xml_set_attr(nodes[i], "ln-possible-metadata", f)
        }


        #####
        # 3: Extract and delete by tagname or fixed position, if possible
        #####

        # Set up master metadata list
        m <- list()

        # By position: the first item is the document count (which we don't use)
        #              and the second is the publication name
        docctstr <- xml_text(nodes[[1]], trim=TRUE)
        xml_set_attr(nodes[1], "ln-parsed-as", "docctstr")
        m[["origin"]] <- xml_text(nodes[[2]], trim=TRUE)
        xml_set_attr(nodes[2], "ln-parsed-as", "origin")

        # By content: looking up by fieldnames
        exflds <- list()
        # We detect, and parse, those fieldnames identified in `fields`.
        # We don't necessarily to anything with these, but we do tag them
        # in the tree: even if we don't put the 'pubcode' into the final
        # metadata, for example, we still don't want it being parsed as part
        # of the article content.
        #
        # xml2 nodesets have reference semantics, so attributes we add
        # to nodes within getfield persist by side effect despite being
        # in a function.
        #
        # Later we take the fields from the exflds list that we want.
        for (field in names(fields)) {
	    exflds[[k]] <- getfield(nodes, field)
        }

        lookup_field <- function(key) {
            if(!is.null(exflds[[key]])) exflds[[key]] else character(0)
        }

        # These are raw string (or character(0)) lookups; those which need
        # splitting, or further parsing are processed later.
        m[["author"]] <- lookup_field("author")
        m[["type"]] <- lookup_field("type")
        m[["section"]] <- lookup_field("section")
        # These are processed later
        m[["language"]] <- lookup_field("language")
        m[["wordcount"]] <- lookup_field("length")
        # These are split later
        m[["intro"]] <- lookup_field("intro")
        m[["subject"]] <- lookup_field("subject")
        m[["coverage"]] <- lookup_field("coverage")
        m[["company"]] <- lookup_field("company")
        m[["stocksymbol"]] <- lookup_field("stocksymbol")
        m[["industry"]] <- lookup_field("industry")

        # Check that we've matched all of the fieldcodes. This doesn't mean
        # we're using them as metadata, just that they're in the fields list
        # and being successfully pulled out.
        # TODO: Many of these for The Mirror are headlines, using the tabloid
        #       convention for reporting speech: "EU: No further Brexit delay".
        # We could filter these by looking for the usual <span class="c9">
        # or whatever? In headlines, I don't think there's usually a style
        # break after the colon.
#        residualcodes <- grepl('^UNKNOWN-', xml_attr(nodes, "ln-possible-metadata", default=""))
#        if (any(residualcodes)) warning("Potential field codes detected which are not currently handled: ",
#                                        gsub('^UNKNOWN-', '', xml_attr(nodes[residualcodes], "ln-possible-metadata")),
#                                        "\n")

        #####
        # 4: Use heuristics to extract copyright, date, heading, and body
        #####

        # Create a vector of text from the nodeset
        vals <- sapply(nodes, xml_text, trim=TRUE)

        # There is no standard parseable fieldname for copyright, but the line
        # starts with "Copyright", and it's at the end of the document, so take
        # the last such line.
        cr <- which(grepl("^Copyright", vals))
        if (any(cr)) {
            m[["rights"]] <- vals[[max(cr)]]
            xml_set_attr(nodes[max(cr)], "ln-parsed-as", "rights")
        } else {
            warning(sprintf("Could not parse copyright notice for article %s. This may indicate a problem with the source data, as LexisNexis copyright notices are nearly universal.\n", id))
            m[["rights"]] <- character(0)
        }

        # Date can be before or after heading: try to detect which is which.
        datepos <- which(grepl(sprintf("(%s).*[0-9]{4}.*(%s)|(%s) [0-9]{2}, [0-9]{4}", months, weekdays, months),
                               vals[1:5], ignore.case=TRUE))
        if(length(datepos) > 0) {
            m[["datetimestamp"]] <- parseDate(vals[datepos[1]])
            xml_set_attr(nodes[datepos[1]], "ln-parsed-as", "datetimestamp")
            headingpos <- setdiff(1:4, datepos[1])[3]
        } else {
            # Can't find it! Guess...
            m[["datetimestamp"]] <- parseDate(vals[3])
            xml_set_attr(nodes[3], "ln-parsed-as", "datetimestamp")
            headingpos <- 4
        }

        if (!xml_has_attr(nodes[headingpos], "ln-parsed-as")) {
            m[["heading"]] <- vals[headingpos]
            xml_set_attr(nodes[headingpos], "ln-parsed-as", "heading")
        } else {
            warning("No heading found for article ", id, "\n")
            heading <- character(0)
        }

        # Position of main text can vary, but we should have tagged everything else parseable.
        contentpos <- which(!xml_has_attr(nodes, "ln-parsed-as"))
        if (!is_seq_contiguous(contentpos)) warning("Non-contiguous paragraphs detected as main text for article", id, "\n")
        content <- sapply(xml_children(nodes[contentpos]), function(x)
                          paste(sapply(xml_children(x), function(y)
                                       paste(trimws(xml_find_all(y, ".//text()")), collapse="\n")),
                                collapse=" "))

        if (is.na(content) || length(content) == 0 || identical(content,"")) {
            warning("No content found for article ", id, "\n")
            content <- ""
        }


        #####
        # 5: Tidy up variables as needed
        #####

        m[["intro"]] <- split_chunk(m[["intro"]])
        m[["subject"]] <- split_chunk(m[["subject"]])
        m[["coverage"]] <- split_chunk(m[["coverage"]])
        m[["company"]] <- split_chunk(m[["company"]])
        m[["stocksymbol"]] <- split_chunk(m[["stocksymbol"]])
        m[["industry"]] <- split_chunk(m[["industry"]])

        # Standardise language, using ISO 639 lookup table where possible
        if(length(m[["language"]]) > 0) {
            langstr <- strsplit(m[["language"]], "; ")[[1]][1]
            m[["language"]] <- ISO_639_2[match(tolower(langstr), tolower(ISO_639_2[["Name"]])), "Alpha_2"]
            if(is.na(m[["language"]]))
                m[["language"]] <- tolower(langstr)
        }
        else {
            m[["language"]] <- character(0)
        }

        # Extract numeric wordcount
        if(length(m[["wordcount"]]) > 0)
            m[["wordcount"]] <- as.integer(regmatches(m[["wordcount"]], regexec("[0-9]+", m[["wordcount"]]))[[1]])
        else
            m[["wordcount"]] <- NA

        # Ensure heuristically extracted items are sane
        m[["author"]] <- if(length(m[["author"]]) > 0 && !is.na(m[["author"]])) m[["author"]] else character(0)
        m[["heading"]] <- if(length(m[["heading"]]) > 0 && !is.na(m[["heading"]])) m[["heading"]] else character(0)
        m[["origin"]] <- if(length(m[["origin"]]) > 0 && !is.na(m[["origin"]])) m[["origin"]] else character(0)

        # Generate a unique id
        pubcode <- lookup_field("pubcode")
        if (length(pubcode) == 0) {
            pubcode <- gsub("[^[:alnum:]]", "", substr(m[["origin"]], 1, 10))
        }
        m[["id"]] <- paste(pubcode,
                           if(!is.na(m[["datetimestamp"]])) strftime(m[["datetimestamp"]], format="%Y%m%d") else "",
                           id, sep="")

        #####
        # 6: Generate and return a PlainTextDocument
        #####

        # XMLSource uses character(0) rather than NA, do the same
        PlainTextDocument(x = content, meta = m)
    }
})

