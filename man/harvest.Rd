\name{harvest}
\alias{oaih_harvest}
\title{OAI-PMH Harvester}
\description{
  Harvest a repository using Open Archives Initiative Protocol for
  Metadata Harvesting (OAI-PMH) requests.
}
\usage{
oaih_harvest(baseurl,
             prefix = "oai_dc", from = NULL, until = NULL, set = NULL,
             transform = TRUE)
}
\arguments{
  \item{baseurl}{a character string giving the base URL of the
    repository.}
  \item{prefix}{a character vector with the formats in which metadata
    should be obtained, or \code{NULL}, indicating all available
    formats.  The default (\code{"oai_dc"}) corresponds to the mandatory
    OAI unqualified Dublin Core metadata schema.}
  \item{from, until}{character strings or \link[base:Dates]{Date} or
    \link[base:DateTimeClasses]{POSIXt date/time objects} giving
    datestamps to be used as lower or upper bounds, respectively, for
    datestamp-based selective harvesting (i.e., only harvest only
    records with datestamps in the given range).  If character, dates
    and times must be encoded using ISO 8601 in either \samp{\%F} or
    \samp{\%FT\%TZ} format (see \code{\link{strptime}}).  The trailing
    \samp{Z} must be used when including time.  OAI-PMH implies
    \acronym{UTC} for data/time specifications.}
  \item{set}{a character vector giving the sets to be used for selective
    harvesting (i.e., only harvest records in the given sets), or
    \code{NULL}.}  
  \item{transform}{a logical indicating whether the OAI-PMH XML results
    to \dQuote{useful} R data structures via \code{\link{oaih_transform}}.
    Default: true.}
}
\details{
  This is a high-level function for conveniently harvesting metadata
  from a repository, allowing specifying several metadata formats or
  sets.  It also maps datestamps specified as R date or date/time
  objects to valid OAI-PMH datestamps according to the granularity of
  the repository.
}

\value{
  If the OAI-PMH request was successful, the result of the request as
  XML or (default) transformed to \dQuote{useful} R data structures.
}
