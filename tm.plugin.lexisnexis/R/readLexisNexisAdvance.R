readLexisNexisAdvance <- FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {
        regexFromFields <- function(field){
            # With LexisNexis Advance the field codes are no longer all uppercase, but
            # (at least in .docx) they are now all consistently at the start of the
            # line, followed by a colon and a non-breaking space (U+00A0).
            paste0('^(', paste0(fields[[field]], collapse='|'), '):\u00a0[ \u00a0]*')
        }

        getParaNumberForField <- function(paras, field, tid) {
            ind <- which(grepl(regexFromFields(field), paras, ignore.case=TRUE))
            if(length(ind) > 1) {
                warning("Multiple matches for field ", field, ": ", tid, ". Choosing the first from ", paras[ind], "\n")
                ind <- min(ind)
            }

            if (length(ind) == 1) return(ind)
            integer(0)
        }

        # The contents of a LexisNexis file are language-dependent,
        # inconsistent, only partially semantically tagged, and don't follow a
        # hierarchical format which is easy for parsing.
        #
        # The basic strategy of this function is as follows:
        # 1) Input and split the content: Header material / body / graphic / classification
        # 2) Use position heuristics to guess the heading, the date, and the
        #    copyright notice etc. (which are not tagged)
        # 3) Extract metadata from the classification section by detecting LN's
        #    slugs such as "Load-Date:" in consistent places at the start of lines
        # 4) Tidy up the variables where needed
        # 5) Make a PlainTextDocument and return it
        #
        # We keep track of which nodes have been processed, and what we got from
        # them, by adding attributes to nodes directly.

        # Temporary id, until we've parsed the date etc.
        tid <- paste0(basename(elem$uri), ":", id)

        # Set up master metadata list
        m <- list()

        #####
        # 1: Parsing
        #####
        paras <- strsplit(elem$content, "\n", fixed=TRUE)[[1]]

        # Trim empty and spurious nodes
        paras <- paras[paras != ""]
        if(tail(paras, 1) == "End of Document")
            paras <- head(paras, -1)

        # Split out "Classification" section
        classification_start <- max(which(grepl('^Classification$', paras)))
        class_paras <- tail(paras, -classification_start)
        paras <- head(paras, classification_start-1)

        # Split out content ("Body") and header sections
        body_start <- min(which(grepl('^Body$', paras)))
        header_paras <- head(paras, body_start-1)
        paras <- tail(paras, -body_start)

        # Split out graphic section (which may be missing)
        poss_graphics <- grepl('^Graphic$', paras)
        if (any(poss_graphics)) {
            graphic_start <- max(which(poss_graphics))
            m[["graphic"]] <- tail(paras, -graphic_start)
            paras <- head(paras, graphic_start-1)
        } else {
            m[["graphic"]] <- character(0)
        }

        content <- paras

        #####
        # 2: Metadata fields by position
        #####
        # The first header item is normally the headline, then the publication,
        #              then the date/edition, then the copyright notice.
        # Sadly, these fields may be missing - especially the publication, which
        # is missing for some Times articles

        # FIXME: handle missing origin line
        cr <- which(grepl("^Copyright", header_paras))
        if (any(cr)) {
            m[["rights"]] <- header_paras[[max(cr)]]
            # Remaining header items are parseable metadata tags
            class_paras <- c(tail(header_paras, -max(cr)), class_paras)
        } else {
            warning("Could not parse copyright notice: ", tid, ". This may indicate a problem with the source data, as LexisNexis copyright notices are nearly universal.\n")
            m[["rights"]] <- character(0)
            class_paras <- c(tail(header_paras, -4), class_paras)
        }

        m[["heading"]] <- header_paras[1]

        if (max(cr) >= 4) {
            m[["origin"]] <- header_paras[2]
            date_ed_str <- header_paras[3]
        } else if (max(cr) == 3) {
            m[["origin"]] <- character(0)
            date_ed_str <- header_paras[2]
        } else {
            m[["origin"]] <- character(0)
            date_ed_str <- ""
            warning("Unable to parse headers: ", tid, "\n")
        }

        # FIXME
        if (max(cr) > 4) warning("Unexpected headers found:", tid, "; ", head(header_paras, max(cr)-1)[-c(1,2,3)], "\n")


        #####
        # 3: Metadata fields by tagname
        #####

        exflds <- list()
        # We detect, and parse, those fieldnames identified in `fields`.
        # We don't necessarily to anything with these. Later we take the fields
        # from the exflds list that we want.
        for (field in names(fields)) {
    	    exflds[[field]] <- getParaNumberForField(class_paras, field, tid)
        }

        lookup_field <- function(key) gsub(regexFromFields(key), "", class_paras[exflds[[key]]], ignore.case=TRUE)

        # These are raw string (or character(0)) lookups; those which need
        # splitting, or further parsing are processed later.
        m[["author"]] <- lookup_field("author")
        m[["type"]] <- lookup_field("type")
        m[["section"]] <- lookup_field("section")
#        m[["graphic"]] <- lookup_field("graphic")
        m[["correction"]] <- lookup_field("correction")
        # These are processed later
        m[["language"]] <- if (!is.na(language)) language else lookup_field("language")
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
        residualcodes <- class_paras[-unlist(exflds)]
        if (length(residualcodes) > 0) warning("Potential field code lines detected which are not currently handled: ",
                                        tid, ".\n",
                                        paste("\t", residualcodes, collapse="\n"),
                                        "\n")


        #####
        # 4: Tidy up variables as needed
        #####

        m[["intro"]] <- split_chunk(m[["intro"]])
        m[["subject"]] <- split_chunk(m[["subject"]])
        m[["coverage"]] <- split_chunk(m[["coverage"]])
        m[["company"]] <- split_chunk(m[["company"]])
        m[["stocksymbol"]] <- split_chunk(m[["stocksymbol"]])
        m[["industry"]] <- split_chunk(m[["industry"]])

        # Standardise language, using ISO 639 lookup table where possible,
        # if we weren't given it explicitly.
        if (is.na(language)) m[["language"]] <- standardiseLanguage(m[["language"]], tid)

        l <- parseDateAndEdition(date_ed_str, tid, language=m[["language"]])
        m[["datetimestamp"]] <- l[[1]]
        m[["edition"]] <- l[[2]]

        # Extract numeric wordcount
        if(length(m[["wordcount"]]) > 0)
            m[["wordcount"]] <- as.integer(regmatches(m[["wordcount"]], regexec("[0-9]+", m[["wordcount"]]))[[1]])
        else
            m[["wordcount"]] <- integer(0)

        # Ensure heuristically extracted items are sane
        m[["author"]] <- if(length(m[["author"]]) > 0 && !is.na(m[["author"]])) m[["author"]] else character(0)
        m[["heading"]] <- if(length(m[["heading"]]) > 0 && !is.na(m[["heading"]])) m[["heading"]] else character(0)
        m[["heading"]] <- m[["heading"]][m[["heading"]] != "No Headline In Original"]
        m[["origin"]] <- if(length(m[["origin"]]) > 0 && !is.na(m[["origin"]])) m[["origin"]] else character(0)

        # No content happens occasionally, almost always because the article is
        # a photo (possibly with a heading and a caption) without a text body.
        if ((all(is.na(content)) || length(content) == 0 || identical(content,"")) &&
            length(m["graphic"]) == 0) {
            warning("No content (and no graphic tag) found: ", tid, "\n")
            content <- ""
        }

        if (is.na(m[["rights"]])) {
            warning("Could not parse copyright notice: ", tid, ". This may indicate a problem with the source data, as LexisNexis copyright notices are nearly universal.\n")
            m[["rights"]] <- character(0)
        }

        if(is.na(m[["datetimestamp"]])) {
            warning("No date: ", tid, ". Falling back to 'LOAD-DATE' field.\n")
            m[["datetimestamp"]] <- parseDateAndEdition(lookup_field("loaddate"), tid, language=m[["language"]])[[1]]
        }

        # Generate a unique id. This is ideally the (unique) publication code
        # plus the date, plus the file sequence number, plus a shortened hash of
        # the metadata to ensure it is actually unique. Not all records
        # have a publication code, though, so fall back as needed.
        pubcode <- lookup_field("pubcode")
        if (length(pubcode) == 0) {
            pubcode <- gsub("[^[:alnum:]]", "", substr(m[["origin"]], 1, 10))
        }
        m[["id"]] <- paste0(pubcode,
                            strftime(m[["datetimestamp"]], format="%Y%m%d"),
                            id,
                            "-",
                            substr(digest::digest(as.character(m),
                                                  algo="md5",
                                                  raw=FALSE),
                                   1, 8)
        )
        #####
        # 6: Generate and return a PlainTextDocument
        #####

        # XMLSource uses character(0) rather than NA, do the same
        PlainTextDocument(x = content, meta = m)
    }
})

