\name{size}
\alias{oaih_size}
\title{OAI-PMH Repository Size}
\description{
  Determine the number of items available for (selective) harvesting in
  an OAI repository.
}
\usage{
oaih_size(baseurl, from = NULL, until = NULL, set = NULL)
}
\arguments{
  \item{baseurl}{a character string giving the base URL of the
    repository.}
  \item{from, until}{character strings or \link[base:Dates]{Date} or
    \link[base:DateTimeClasses]{POSIXt date/time objects} giving
    datestamps to be used as lower or upper bounds, respectively, for
    datestamp-based selective harvesting (i.e., only consider records
    with datestamps in the given range).  If character, dates and times
    must be encoded using ISO 8601 in either \samp{\%F} or
    \samp{\%FT\%TZ} format (see \code{\link{strptime}}).  The trailing
    \samp{Z} must be used when including time.  OAI-PMH implies
    \acronym{UTC} for data/time specifications.}
  \item{set}{a character vector giving the sets to be considered for
    selective harvesting (i.e., only consider records in the given
    sets), or \code{NULL}.}  
}
\details{
  Determining the number of items without actually harvesting these is
  only possible if the repository's flow control mechanism provides
  \code{resumptionToken} elements with \code{completeListSize}
  attributes (see
  \url{http://www.openarchives.org/OAI/openarchivesprotocol.html}), or
  flow control is not applied when listing identifiers in the selected
  range.
}
\value{
  A numeric giving the number of items available for (selective)
  harvesting, or \code{NA_real_} if the number could not be determined
  without harvesting.
}
\examples{
oaih_size("http://www.jstatsoft.org/oai")
}
