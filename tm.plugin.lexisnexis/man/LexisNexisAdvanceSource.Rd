\name{LexisNexisAdvanceSource}
\alias{LexisNexisAdvanceSource}
\alias{getElem.LexisNexisAdvanceSource}
\alias{eoi.LexisNexisAdvanceSource}
\title{LexisNexis Advance Source}
\description{
  Construct a source for an input containing a set of articles
  exported from LexisNexis Advance in the .docx format.
}
\usage{
  LexisNexisAdvanceSource(x)
}
\arguments{
  \item{x}{Either a character identifying the file or a connection.}
}
\details{
  This function imports the body of the articles, but also sets several
  meta-data variables on individual documents where data are available:
  \itemize{
    \item \code{datetimestamp}: The publication date.
    \item \code{heading}: The title of the article.
    \item \code{origin}: The newspaper the article comes from.
    \item \code{edition}: The edition of the newspaper the article comes from.
    \item \code{author}: The bylined author(s) of the article.
    \item \code{intro}: The short introduction accompanying the article.
    \item \code{section}: The part of the newspaper containing the article.
    \item \code{graphic}: The text (captions etc.) for any graphics accompanying
      the article.
    \item \code{subject}: One or several keywords defining the subject.
    \item \code{coverage}: One or several keywords identifying the covered regions.
    \item \code{company}: One or several keywords identifying the covered companies.
    \item \code{stocksymbol}: One or several keywords identifying the stock exchange
      symbols of the covered companies.
    \item \code{industry}: One or several keywords identifying the covered industries.
    \item \code{correction}: Any subsequent corrections made to the article.
    \item \code{type}: The type of source from which the document originates.
    \item \code{wordcount}: The number of words in the article.
    \item \code{publisher}: The publisher of the newspaper.
    \item \code{rights}: The copyright information associated with the article.
    \item \code{language}: This information is set automatically if
      \code{readerControl = list(language = NA)} is passed (see the example below).
      Else, the language specified manually is set for all articles. If omitted,
      the default, "en", is used.
  }

  Please note that dates are not guaranteed to be parsed correctly
  if the machine from which the input file was exported
  uses a locale different from that of the machine where it is read.

  Currently, only input files saved in English and French are supported.
  Please send the maintainer examples of LexisNexis files in your
  language if you want it to be supported.
}
\value{
  An object of class \code{LexisNexisAdvanceSource} which extends the class
  \code{Source} representing set of articles from LexisNexis.
}
\author{
  Milan Bouchet-Valat
}
\examples{
    library(tm)
    file <- system.file("texts", "lexisnexis_advance_test_en.docx",
                        package = "tm.plugin.lexisnexis")
    corpus <- Corpus(LexisNexisAdvanceSource(file))

    # See the contents of the documents
    inspect(corpus)

    # See meta-data associated with first article
    meta(corpus[[1]])
}
\seealso{
  \code{\link{readLexisNexisAdvance}} for the function actually parsing
  individual articles.

  \code{\link{LexisNexisSource}} for handling articles from LexisNexis
  in \acronym{HTML} format.

  \code{\link{getSources}} to list available sources.
}
