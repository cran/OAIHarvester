oaih_size <-
function(baseurl, from = NULL, until = NULL, set = NULL)
{
    request <-
        c("verb=ListIdentifiers&metadataPrefix=oai_dc",
          ## Note that from or until could be date/time objects ...
          if(!is.null(from)) sprintf("from=%s", from),
          if(!is.null(until)) sprintf("until=%s", until),
          if(!is.null(set)) sprintf("set=%s", set))

    nodes <- tryCatch(OAI_PMH_issue_request(baseurl, request),
                      error = identity)
    if(inherits(nodes, "error")) {
        if(grepl("noRecordsMatch", conditionMessage(nodes)))
            return(0)
        else
            stop(nodes)
    }
    
    result <- OAI_PMH_get_result(nodes)
    kids <- xml_children(result)
    size <- length(kids)
    last <- kids[[size]]
    if(xml_name(last) == "resumptionToken") {
        size <- as.numeric(xml_attr(last, "completeListSize"))
    }

    size
}
