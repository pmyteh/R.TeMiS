readLexisNexisAdvance <- FunctionGenerator(function(elem, language, id) {
    function(elem, language, id) {

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

        # Temporary id, until we've parsed the date etc.
        tid <- paste0(basename(elem$uri), ":", id)

        # Set up master metadata list
        m <- list()

        #####
        # 1: Parsing
        #####

        # We try to detect valid sections from the bottom, splitting them out
        # and shortening the rump 'paras' vector as we go. Any of the sections
        # except the header section could be missing.
        
        paras <- strsplit(elem$content, "\n", fixed=TRUE)[[1]]
        # Trim empty and spurious nodes
        paras <- paras[paras != ""]
        if(tail(paras, 1) == "End of Document")
            paras <- head(paras, -1)

        # Split out the optional "Classification" section at the bottom.
        if (any(grepl('^Classification$', paras))) {
            classification_start <- max(which(grepl('^Classification$', paras)))
            class_paras <- tail(paras, -classification_start)
            paras <- head(paras, classification_start-1)
        } else if (any(grepl('^Load-Date:', paras))) {
            classification_start <- max(which(grepl('^Load-Date:', paras)))
            class_paras <- tail(paras, 1-classification_start)
            paras <- head(paras, classification_start-1)
        } else stop("can't detect a 'Classification' section or a Load-Date: ", tid, ". Giving up.\n")

        # Split out the optional graphic section. If there are multiple lines
        # starting with 'Graphic', we choose the latest. This is not perfect.
        graphic_start <- which(grepl('^Graphic$', paras))
        if (length(graphic_start) > 0) {
            graphic_start <- max(graphic_start)
            m[["graphic"]] <- tail(paras, -graphic_start)
            paras <- head(paras, graphic_start-1)
        } else {
            m[["graphic"]] <- character(0)
        }
        
        # Split out the optional content ("Body") section. If there are multiple
        # lines starting with 'Body' we choose the first. This is also not
        # perfect.
        body_start <- which(grepl('^Body$', paras))
        if (length(body_start) > 0) {
            body_start <- min(body_start)
            content <- tail(paras, -body_start)
            paras <- head(paras, body_start-1)
        } else {
            content <- character(0)
        }

        # The header section is what's left (at the start).
        header_paras <- paras

        if (length(content) == 0 && length(m[["graphic"]]) == 0) {
            message("Can't detect either a 'Body' section or a 'Graphic' section: ", tid, ".\n")
        }
        
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

        regexFromFields <- function(field) {
            # With LexisNexis Advance the field codes are no longer all uppercase, but
            # (at least in .docx) they are now all consistently at the start of the
            # line, followed by a colon and a non-breaking space (U+00A0).
            paste0('^(', paste0(fields[[field]], collapse='|'), '):\\h+')
            #            paste0('^(', paste0(fields[[field]], collapse='|'), '):\u00a0[ \u00a0]*')
        }
        
        getParaNumbersForField <- function(paras, field, tid) {
            ind <- which(grepl(regexFromFields(field), paras, ignore.case=TRUE, perl=TRUE))
            if(length(ind) > 1) {
                warning("Multiple matches for field ", field, ": ", tid, ". Choosing the first from ", paras[ind], "\n")
                ind <- min(ind)
            }
            
            if (length(ind) == 0) return(integer(0))
            
            extralines <- 0
            if (ind != length(paras)) for (i in (ind+1):length(paras)) {
                # We're assuming that 'run-on' fields, where the content
                # continues on to the next paragraph, don't have the ': '
                # structure that potentially indicates a tag. There's no perfect
                # solution to this, as we don't have an exhaustive list of tags.
                # If we falsely exclude something here, and it *doesn't* match a
                # tag that we know about, a warning will be thrown later.
                if (!grepl(':\\h+', paras[i], ignore.case=TRUE, perl=TRUE)) {
                    extralines <- extralines + 1
                } else break
            }
#            if (extralines > 0) warning(tid, ": extra lines detected for ", field, ": ",
#                                        paras[ind:ind+extralines], '\n')
            if (extralines > 0) return(ind:(ind+extralines))
            
            ind
        }
        
        
        exflds <- list()
        # We detect, and parse, those fieldnames identified in `fields`.
        # We don't necessarily to anything with these. Later we take the fields
        # from the exflds list that we want.
        for (field in names(fields)) {
    	    exflds[[field]] <- getParaNumbersForField(class_paras, field, tid)
        }

        lookup_field <- function(key) {
            content <- gsub(regexFromFields(key), "", class_paras[exflds[[key]]], ignore.case=TRUE, perl=TRUE)
            paste0(content, collapse='; ')
        }

        # These are raw string (or character(0)) lookups; those which need
        # splitting, or further parsing are processed later.
        m[["author"]] <- lookup_field("author")
        m[["type"]] <- lookup_field("type")
        m[["section"]] <- lookup_field("section")
#        m[["graphic"]] <- lookup_field("graphic")
        m[["correction"]] <- lookup_field("correction")
        # These are processed later. We do not rely on the 'language' field fed
        # in via the readerControl structure here, because VCorpus sets this to
        # 'en' by default, which is a major footgun. We pick it up later (for
        # use in parsing dates etc., not for actually putting in the document
        # metadata) if we don't have anything.
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
        residualcodes <- class_paras[-unlist(exflds)]
        if (length(residualcodes) > 0) message("Potential field code lines ",
                                               "detected which are not ",
                                               "currently handled (or could ",
                                               "be part of the previous ",
                                               "field): ", tid, ".\n",
                                               paste("\t", residualcodes,
                                                     collapse="\n"))


        #####
        # 4: Tidy up variables as needed
        #####

        m[["intro"]] <- split_chunk(m[["intro"]])
        m[["subject"]] <- split_chunk(m[["subject"]])
        m[["coverage"]] <- split_chunk(m[["coverage"]])
        m[["company"]] <- split_chunk(m[["company"]])
        m[["stocksymbol"]] <- split_chunk(m[["stocksymbol"]])
        m[["industry"]] <- split_chunk(m[["industry"]])

        # Standardise language, using ISO 639 lookup table where possible. Iff
        # we can't find a language in the document, fall back on whatever we
        # were given in the readerControl structure.
        m[["language"]] <- standardiseLanguage(m[["language"]], tid)

        # Extract datetimestamp and edition
        bestguesslang <- if (length(m[["language"]] > 0)) m[["language"]] else language
        l <- parseDateAndEdition(date_ed_str, tid, language=bestguesslang)
        m[["datetimestamp"]] <- l[[1]]
        m[["edition"]] <- l[[2]]

        # Extract numeric wordcount
        m[["wordcount"]] <- wordcountFromString(m[["wordcount"]])
        
        # Extract page number from section string if possible
        l <- parsePageAndSection(m[["section"]], bestguesslang, tid)
        m[["page"]] <- l[["page"]]
        m[["section"]] <- l[["section"]]

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

        if (length(m[["rights"]]) == 0 || is.na(m[["rights"]])) {
            warning("Could not parse copyright notice: ", tid, ". This may indicate a problem with the source data, as LexisNexis copyright notices are nearly universal.\n")
            m[["rights"]] <- character(0)
        }

        if(length(m[["datetimestamp"]]) == 0 || is.na(m[["datetimestamp"]])) {
            warning("No date: ", tid, ". Falling back to 'LOAD-DATE' field.\n")
            m[["datetimestamp"]] <- parseDateAndEdition(lookup_field("loaddate"), tid, language=m[["language"]])[[1]]
        }

        # Generate unique ID
        m[["id"]] <- idFromMetadata(m, lookup_field("pubcode"), id)

        #####
        # 6: Generate and return a PlainTextDocument
        #####

        # XMLSource uses character(0) rather than NA, do the same
        PlainTextDocument(x = content, meta = m)
    }
})

