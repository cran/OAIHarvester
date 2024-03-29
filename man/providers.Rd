\name{providers}
\alias{oaih_providers}
\title{OAI-PMH Providers}
\description{
  Names, base URLs and identifiers of registered and validated OAI
  conforming metadata providers.
}
\usage{
oaih_providers()
}
\details{
  Information is extracted from
  \url{https://www.openarchives.org/Register/BrowseSites}
  (as the XML formatted list of base URLs of registered data providers
  from \url{https://www.openarchives.org/pmh/registry/ListFriends} does not
  provide repository names), and cached for the current R session.
}
\value{
  A character data frame with variables \code{name}, \code{baseurl} and
  \code{identifier} providing the repository names, base URLs and OAI
  identifier (see
  \url{https://www.openarchives.org/OAI/2.0/guidelines-oai-identifier.htm}).
}
