LexisNexisAdvanceSource <- function(x) {
    # LexisNexis Advance files have a different format, and HTML output is no
    # longer available :-/ textreadr should handle any of the full-text
    # document formats, with some minor edits though: .pdf, .docx, and .rtf.
    lines <- textreadr::read_docx(x)
    
    docsrx <- '^Documents \\(([0-9]+)\\)'
    # If we have a header page, trim it. This could be usefully internationalised.
    if (grepl('^Date and Time:', lines[1]) &&
        grepl('^Job Number:', lines[2]) &&
        grepl(docsrx, lines[3])) {
        ndocs <- as.integer(gsub(docsrx, '\\1', lines[3]))
        
        if (ndocs > 1) {
            nheaderrowsperdoc <- grep('^2.', lines)[[1]] - grep('^1.', lines)[[1]]
            # Trim
            lines <- tail(lines, n=-3-(ndocs * nheaderrowsperdoc))
            
        } else warning("Don't know how to handle header pages where there is only one document.")
        

    }

    # Document starts: First line, plus (by pushing the vector forward by one
    # and trimming the end) the lines following "End of Document"
    newdocs <- c(1, head(grepl('^End of Document', lines, ignore.case=TRUE), -1))

    # Call as.character() to remove useless names and get a vector instead of a
    # 1d array
    content <- as.character(tapply(lines, cumsum(newdocs), paste, collapse="\n"))

    # Get rid of short empty sections
    content <- content[nchar(content) > 200]

    # If LexisNexis has generated an error 'document' then we won't be able to
    # handle it; warn and drop
    errtexts <- grepl("We are sorry but there is an error in this document and it is not possible to display it.",
                      content,
                      fixed=TRUE)
    if(any(errtexts)) {
        warning(x, ": LexisNexis failed to provide some documents; skipping number(s) ",
                paste0(which(errtexts), collapse=", "),
                "\n")
        content <- content[!errtexts]
    }

    SimpleSource('', length(content),
                 content=content, uri=x,
                 reader=readLexisNexisAdvance, class="LexisNexisAdvanceSource")
}

getElem.LexisNexisAdvanceSource <- function(x) list(content = x$content[[x$position]], uri = x$uri)
