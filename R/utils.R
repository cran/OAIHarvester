## Helper functions.

## <NOTE>
## Unlike package 'XML', package 'xml2' does not provide subscripting
## XML nodes by name (the XML names of the kids), as subnodes can always
## be found via XPATH expressions.  As this also uses the namespace
## information, one needs to use names explicitly qualified with the ns
## prefix.  We could use xml_ns_strip() to strip the namespace info we
## get, or needs to be explicit (using 'd1' as prefix for the default
## "http://www.openarchives.org/OAI/2.0/" namespace) ... or use the
## little helper below.
## </NOTE>

.xml_children_named <-
function(x, name)
{
    kids <- xml_children(x)
    kids[xml_name(kids) == name]
}

.xml_child_named <-
function(x, name, default = xml_missing())
{
    kids <- .xml_children_named(x, name)
    if(length(kids)) kids[[1L]] else default
}

.get_all_any_nodes <-
function(x, name)
    lapply(.xml_children_named(x, name), xml_child, 1L)
## So after
##   require("OAIHarvester")
##   baseurl <- "http://epub.wu.ac.at/cgi/oai2"
##   x <- oaih_identify(baseurl, transform = FALSE)
## the description elements can be obtained via
##   xml_find_all(x, "./d1:description")
## or
##   .xml_children_named(x, "description")
## and their "contents" via
##   lapply(xml_find_all(x, "./d1:description"), xml_child, 1L)
## or
##   lapply(.xml_children_named(x, "description"), xml_child, 1L)
## or
##   .get_all_any_nodes(x, "description")

.get_one_any_node <-
function(x, name)
{
    kids <- .xml_children_named(x, name)
    if(length(kids)) {
        kids <- xml_children(kids[[1L]])
        if(length(kids))
            kids[[1L]]
        else
            NULL
    }
    else
        NULL
}
## There may be no element named with the given 'name', or it may be
## empty: in both cases we return NULL, otherwise the (first) child of
## the first element.
## Note that after e.g.
##   require("OAIHarvester")
##   baseurl <- "http://epub.wu.ac.at/cgi/oai2"
##   x <- oaih_get_record(baseurl, "oai:epub.wu-wien.ac.at:4274",
##                        transform = FALSE)
## (which is a deleted record and hence has no metadata), when finding 
## the first metadata element via
##   xml_find_first(x, "./d1:metadata")
## gives xml_missing(), which we could test for using is.na().
    
.get_value_of_any_text_nodes <-
function(x, names = NULL)
{        
    all_kids <- lapply(x, xml_children)
    all_names <- lapply(all_kids, xml_name)
    if(is.null(names)) {
        names <- unique(unlist(all_names))
    } 
    out <- Map(function(k, n) {
                   split(xml_text(k), factor(n, names))[names]
               },
               all_kids,
               all_names)
    matrix(unlist(out, recursive = FALSE),
           nrow = length(x), ncol = length(names),
           byrow = TRUE, dimnames = list(NULL, names))
}

.get_value_of_unique_text_nodes <-
function(x, names = NULL)
{
    all_kids <- lapply(x, xml_children)
    all_names <- lapply(all_kids, xml_name)
    if(is.null(names)) {
        names <- unique(unlist(all_names))
    }
    out <- lapply(all_kids,
                  function(kids) {
                      as.list(xml_text(kids)[match(names, xml_name(kids))])
                  })
    matrix(unlist(out, recursive = FALSE),
           nrow = length(x), ncol = length(names),
           byrow = TRUE, dimnames = list(NULL, names))
}    

.OAI_PMH_UTC_date_stamp <-
function(x, times_ok = TRUE)
{
    ## This could also be used in oaih_list_records(), either with a
    ## times_ok = TRUE default (users should know what they do in case
    ## they give a date/time object ...).  Alternatively, in case users
    ## give from/to and we want to be nice, we could query the
    ## repository ... 
    
    if(inherits(x, "POSIXt")) {
        ## Convert to GMT.
        x <- as.POSIXlt(x, tz = "GMT")
        x <- strftime(x, if(times_ok) "%FT%TZ" else "%F")
    }
    else {
        x <- as.character(x)
        if(!times_ok && (nchar(x) > 10L)) {
            warning("Repository only supports dates.")
            x <- substring(x, 1L, 10L)
        }
    }
    
    x
}

html_tables <-
function(doc, which = integer())
{
    if(!inherits(doc, "xml_document"))
        doc <- read_html(doc)
    nodes <- xml_find_all(doc, "//table")
    if(length(which))
        nodes <- nodes[which]
    tables <- lapply(nodes, html_table_body)
    names(tables) <-
        vapply(nodes, html_table_name, "")
    tables
}

html_table_body <-
function(node)
{
    header <- NULL
    drop_first_row <- FALSE
    nodes <- xml_find_all(node, "./thead")
    if(length(nodes))
        header <- trimws(xml_text(xml_find_all(nodes[[1L]], ".//th")))
    nodes <- xml_find_all(node, "./tbody")
    if(length(nodes))
        node <- nodes[[1L]]
    if(is.null(header)) {
        nodes <- xml_find_all(node, "./tr[1]/th")
        if(length(nodes)) {
            header <- trimws(xml_text(nodes))
            drop_first_row <- TRUE
        }
    }
    rows <- xml_find_all(node, ".//tr")
    if(drop_first_row) rows <- rows[-1]
    elts <- lapply(rows,
                   function(row) {
        trimws(xml_text(xml_find_all(row, "./th | ./td")))
    })
    if(!length(elts)) return(NULL)
    ind <- seq_len(max(vapply(elts, length, 0L)))
    tab <- do.call(rbind, lapply(elts, `[`, ind))
    if(length(header) == ncol(tab))
        colnames(tab) <- header
    tab
}

html_table_name <-
function(node)
{
    id <- xml_attr(node, "id")
    if(!is.na(id))
        return(id)
    nodes <- xml_find_all(node, "./caption")
    if(length(nodes))
        return(xml_text(nodes[[1L]]))
    ""
}
