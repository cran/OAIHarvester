.GET_default <-
function(url)
{
    h <- basicHeaderGatherer()
    .opts <- list(headerfunction = h$update)
    ## <NOTE>
    ## Hard-wire UTF-8 for now: as of 2010-07-03, otherwise e.g.
    ##   u <- "http://epub.wu.ac.at/cgi/oai2?verb=GetRecord&identifier=oai:epub.wu-wien.ac.at:52&metadataPrefix=oai_dc"
    ##   x <- unlist(strsplit(RCurl::getURL(u, header = FALSE), "\n"))
    ## has encoding problems ...
    ## Hard-wire .mapUnicode = FALSE as well: on 2013-04-20,
    ##   u <-"http://projecteuclid.org/DPubS?verb=ListRecords&resumptionToken=until%253d2011-09-23%2526from%253d2011-09-23%2526metadataPrefix%253doai_dc%2526token%253deuclid.kmj%252f1138846414"
    ##   RCurl::getURL(u)
    ## fails otherwise ...
    body <- getURL(url, .opts = .opts,
                   .encoding = "UTF-8", .mapUnicode = FALSE)
    ## </NOTE>
    list(header = h$value(), body = body)
}

.GET_using_gzip_compression <-
function(url)
{
    h <- basicHeaderGatherer()
    .opts <- list(headerfunction = h$update,
                  httpheader = c("Accept-Encoding" = "gzip"))
    bin <- getBinaryURL(url, .opts = .opts)
    ## Ensure that we actually got something gzipped:
    header <- h$value()
    con <- if(grepl("gzip", header["Content-Encoding"])) {
        gzcon(rawConnection(bin))
    } else
        rawConnection(bin)
    on.exit(close(con))
    body <- readLines(con, warn = FALSE, encoding = "UTF-8")
    list(header = header, body = body)
}

GET <-
local({
    val <- .GET_default
    function(new, gzip = FALSE) {
        if(!missing(new))
            val <<- new
        else if(!missing(gzip)) {
            if(identical(gzip, TRUE))
                val <<- .GET_using_gzip_compression
            else
                val <<- .GET_default
        } else {
            val
        }
    }
})
