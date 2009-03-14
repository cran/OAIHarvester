## OAI-PMH infrastructure.

## See http://www.openarchives.org/OAI/openarchivesprotocol.html

OAI_PMH_issue_request <-
function(baseurl, request)
{
    request <- paste(request, collapse = "&")
    ## <FIXME>
    ## Could also use
    ##    con <- gzcon(url(......))
    ## but does this necessarily work if the repository does not feature
    ## gzip compression, and does it make a real difference if it does?
    con <- url(URLencode(paste(baseurl, request, sep = "?")))
    ## </FIXME>
    on.exit(close(con))
    lines <- readLines(con, warn = FALSE)
    ## http://www.openarchives.org/OAI/2.0/openarchivesprotocol.htm says
    ## that the XML responses to OAI-PMH requests have the following
    ## common markup:
    ## * The first tag output is an XML declaration where the version is
    ##   always 1.0 and the encoding is always UTF-8.
    ## * The remaining content is enclosed in a root element with the
    ##   name OAI-PMH.  This element must have three attributes that
    ##   define the XML namespaces used in the remainder of the response
    ##   and the location of the validating schema.
    ## * The first two children of the root element are always
    ##   ** responseDate (a UTCdatetime indicating the time and date
    ##      that the response was sent).
    ##   ** request (indicating the protocol request that generated this
    ##      response).
    ## * The third child of the root element is either:
    ##   ** an error element that must be used in case of an error or
    ##      exception condition;
    ##   ** an element with the same name as the verb of the respective
    ##      OAI-PMH request.
    ## See http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd.

    ## We will refer to the third child as the "result" in the non-error
    ## case.

    ## But what should we return?
    ## In case of an error, most likely an error condition.
    ## Hmm.  As this is the "basic" stuff, perhaps just the XML parse
    ## tree.  Let specific request issuers handle the resumptionToken
    ## accumulation as necessary ...

    nodes <- xmlTreeParse(lines, asText = TRUE)

    ## It would be nice to use internal nodes and xmlPathApply for
    ## more efficiently extracting nodes ... 

    result <- OAI_PMH_get_result(nodes)

    if(xmlName(result) == "error") {
        msg <- sprintf("Received condition '%s'",
                       xmlAttrs(result)["code"])
        txt <- xmlValue(result)
        if(nzchar(txt))
            msg <- paste(msg, sprintf("with diagnostic:\n%s", txt))
        stop(msg)
    }

    nodes
    
}

## Get request verb from OAI-PMH request response
OAI_PMH_get_verb <-
function(nodes)
    xmlAttrs(xmlRoot(nodes)[["request"]])["verb"]

## Get result from OAI-PMH request response.
OAI_PMH_get_result <-
function(nodes)    
    xmlRoot(nodes)[[3L]]

OAI_PMH_gather_request_results <-
function(baseurl, request)
{
    ## Gather request results unless complete (no more resumption
    ## tokens) and return the aggregated "result" of the request(s).
    
    nodes <- OAI_PMH_issue_request(baseurl, request)
    ## Errors would have been thrown.
    result <- OAI_PMH_get_result(nodes)
    verb <- OAI_PMH_get_verb(nodes)
    kids <- xmlChildren(result)
    repeat {
        size <- length(kids)
        ## Assume that the resumption token comes last.
        last <- kids[[size]]
        done <- if(xmlName(last) != "resumptionToken")
            TRUE
        else {
            ## Drop resumption token from results.
            kids <- kids[-size]
            ## Done iff the resumption token is "empty".
            !length(token <- xmlValue(last))
        }
        if(done) break
        nodes <-
            OAI_PMH_issue_request(baseurl,
                                  sprintf("verb=%s&resumptionToken=%s",
                                          verb, token))
        kids <- c(kids, xmlChildren(OAI_PMH_get_result(nodes)))
    }
    
    xmlChildren(result) <- kids
    result
}

