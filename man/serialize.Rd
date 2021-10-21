\name{serialize}
\alias{oaih_read_RDS}
\alias{oaih_save_RDS}
\alias{oaih_str_to_xml}
\alias{oaih_xml_to_str}
\title{Serialization for OAI-PMH Objects}
\description{
  Functions to write a single OAI-PMH object to a file, and to restore
  it, and to perform the necessary conversions of XML objects to and
  from strings.
}
\usage{
oaih_read_RDS(file, ...)
oaih_save_RDS(x, ...)
oaih_str_to_xml(x)
oaih_xml_to_str(x)
}
\arguments{
  \item{x}{an \R object.}
  \item{file}{a connection or the name of the file where the \R object is 
    saved to.}
  \item{...}{arguments to be passed to
    \code{\link[base]{readRDS}} (\code{oaih_read_RDS})
    \code{\link[base]{saveRDS}} (\code{oaih_save_RDS}).}
}
\details{
  The OAI-PMH objects obtained by OAI-PMH requests (e.g.,
  \code{\link{oaih_list_records}}) and 
  subsequent transformations (\code{\link{oaih_transform}}) are 
  made up of both character vectors and XML nodes from package
  \CRANpkg{xml2}, with the latter lists of external pointers.  Thus,
  serialization does not work \dQuote{out of the box}, and in fact using
  refhooks in calls to \code{readRDS} or \code{saveRDS} does not work
  either (as one needs to (de)serialize a list of pointers, and not a
  single one).  We thus provide helper functions to (recursively)
  (de)serialize the XML objects to/from strings, and to pre-process \R
  objects before saving to a file and post-process after restoring from
  a file.
}
\examples{
tryCatch({
## Run inside tryCatch() so that checks fail gracefully if OAI-PMH
## requests time out or fail otherwise.
baseurl <- "https://epub.wu.ac.at/cgi/oai2"
x <- oaih_identify(baseurl)
## Now 'x' is a list of character vectors and XML nodes:
x
## To save to a file and restore:
f <- tempfile()
oaih_save_RDS(x, file = f)
y <- oaih_read_RDS(f)
all.equal(x, y)
## Equivalently, we can directly pre-process before saving and
## post-process after restoring:
saveRDS(oaih_xml_to_str(x), f)
z <- oaih_str_to_xml(readRDS(f))
all.equal(y, z)
##
}, error = identity)
}
