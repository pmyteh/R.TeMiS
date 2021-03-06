\name{readLexisNexisAdvance}
\alias{readLexisNexisAdvance}
\title{Read in a LexisNexis Advance article in the .docx format}
\description{
  Read in an article exported from LexisNexis Advance in the \acronym{.docx}
  format.
}
\usage{
  readLexisNexisAdvance(elem, language, id)
}
\arguments{
  \item{elem}{A \code{list} with the named element \code{content} which
    must hold the document to be read in.}
  \item{language}{A \code{character} vector giving the text's language.
    If set to \code{NA}, the language will automatically be set to the value
    reported in the document (which is usually correct).}
  \item{id}{A \code{character} vector representing a unique identification
    string for the returned text document. This is only used for error
    reporting and is replaced by a generated id in the resulting document.}
}
\value{
   A \code{PlainTextDocument} with the contents of the article and the
   available meta-data set.
}
\seealso{
  \code{\link{getReaders}} to list available reader functions.
}
\author{
  Tom Nicholls, based on the readLexisNexisHTML code by Milan Bouchet-Valat
}
