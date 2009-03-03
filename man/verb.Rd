\name{verb}
\alias{oaih_get_record}
\alias{oaih_identify}
\alias{oaih_list_identifiers}
\alias{oaih_list_metadata_formats}
\alias{oaih_list_records}
\alias{oaih_list_sets}
\title{OAI-PMH Verb Functions}
\description{
  Perform Open Archives Initiative Protocol for Metadata Harvesting
  (OAI-PMH) requests for harvesting repositories.
}
\usage{
oaih_get_record(baseurl, identifier, prefix = "oai_dc", transform = TRUE)
oaih_identify(baseurl, transform = TRUE)
oaih_list_identifiers(baseurl, prefix = "oai_dc", from = NULL,
                      until = NULL, set = NULL, transform = TRUE)
oaih_list_metadata_formats(baseurl, identifier = NULL, transform = TRUE)
oaih_list_records(baseurl, prefix = "oai_dc", from = NULL,
                  until = NULL, set = NULL, transform = TRUE)
oaih_list_sets(baseurl, transform = TRUE)
}
\arguments{
  \item{baseurl}{a character string giving the base URL of the
    repository.}
  \item{identifier}{a character string giving the unique identifier for
    an item in a repository.}
  \item{prefix}{a character string to specify the metadata format in
    OAI-PMH requests issued to the repository.  The default
    (\code{"oai_dc"}) corresponds to the mandatory OAI unqualified
    Dublin Core metadata schema.}
  \item{from, until}{character strings giving datestamps to be used as
    lower or upper bounds, respectively, for datestamp-based selective
    harvesting (i.e., only harvest only records with datestamps in the
    given range).  Dates and times must be encoded using ISO 8601 in
    either \samp{\%F} or \samp{\%FT\%TZ} format (see
    \code{\link{strptime}}).  The trailing \samp{Z} must be used when 
    including time.  OAI-PMH implies UTC for data/time specifications.}
  \item{set}{a character string giving a set to be used for selective
    harvesting (i.e., only harvest records in the given set).}
  \item{transform}{a logical indicating whether the OAI-PMH XML results
    to ``useful'' R data structures via \code{\link{oaih_transform}}.
    Default: true.}
}
\value{
  If the OAI-PMH request was successful, the result of the request as
  XML or (default) transformed to ``useful'' R data structures.
}
\examples{
## Harvest Epub@WU metadata.
baseurl <- "http://epub.wu-wien.ac.at/dyn/OAI/oaicgi.pl"
## Identify.
oaih_identify(baseurl)
## List metadata formats.
oaih_list_metadata_formats(baseurl)
## List sets.
oaih_list_sets(baseurl)
## List records in the 'theses' set.
x <- oaih_list_records(baseurl, set = "theses")
## Extract the metadata.
m <- oaih_transform(x[, "metadata"])
## Cross-tabulate year and language.
table(Year = unlist(m[, "date"]),
      Language = unlist(m[, "language"]))
}
