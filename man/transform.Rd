\name{transform}
\alias{oaih_transform}
\title{Transform OAI-PMH XML Results}
\description{
  Transform OAI-PMH XML results to ``useful'' R data structures (lists
  of character vectors or XML nodes) for further processing or analysis.
}
\usage{
oaih_transform(x)
}
\arguments{
  \item{x}{an XML node, or a list of character vectors or XML nodes.}
}
\details{
  In a ``list context'', i.e., if \code{x} conceptually contains
  information on several cases, transformation gives a ``list matrix''
  (a list of character vector or XML node observations with a dim
  attribute) providing a rectangular case by variables data layout;
  otherwise, a list of variables.  See the vignette for details.
}
\value{
  A list of character vectors or XML nodes, arranged as a matrix in the
  ``list context''.
}
\examples{
if(!is.null(nsl("epub.wu-wien.ac.at"))) {
    baseurl <- "http://epub.wu-wien.ac.at/dyn/OAI/oaicgi.pl"
    ## Get a single record to save bandwidth.
    x <- oaih_get_record(baseurl,
                         "oai:epub.wu-wien.ac.at:epub-wu-01_c4e",
                         transform = FALSE)
    ## The result of the request is a single OAI-PMH XML <record> node:
    x
    ## Transform this (turning identifier, datestamp and setSpec into
    ## character data):
    x <- oaih_transform(x)
    x    
    ## This has its metadata in the default Dublin Core form, encoded in
    ## XML.  Transform these to character data:
    oaih_transform(x$metadata)
}
}
