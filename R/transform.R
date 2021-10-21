## Transformations.

## The OAI-PMH request issues can either return the "raw" XML results,
## or a suitable transformation thereof, which in turn can be obtained
## by calling oaih_transform() on the raw XML.

## What should the XML results be transformed into?
##
## Earlier versions were based on the following design:
##
## OAI-PMH requests that describe a single "entity" (repository for
## Identify(), record for GetRecord()) give a list.  Requests that
## possibly describe multiple entities (all ListSomething() requests)
## give data frames.
## Elements of entities are mapped as follows.
## Elements which are strings occurring exactly once: character vectors.
## Elements which are strings occurring arbitrarily often: lists of
## character vectors (NULL if missing).
## Elements which are XML "nodes" occurring at most once: lists of XML
## nodes (or NULL if missing).
##
## This complies with the "usual" approach of arranging rectangular,
## case by variables data in data frames (and allows to distinguish the
## XML sequence text-type elements which must occur *exactly* once from
## others).  However, it arranges data by "columns" (variables) which is
## somewhat unnatural when processing data by "rows".  In particular, a
## row from the data frame corresponding to a ListRecords() request would
## be different from the list transformation of the corresponding
## GetRecord() request.  Or equivalently, the data frame cannot simply
## be built from the individual lists via rbind().
##
## We thus provide a design which, in the rectangular cases, treats rows
## and columns symmetrically by arranging data in a "list matrix" (a
## list with a dim attribute).  As matrix subscripting drops dimensions
## when a single row or column is selected, one gets a "simple" list
## (without a dim attribute) in these cases.  Transforming in a list
## context (ListSomething() requests, or lists of same-kind XML nodes)
## gives a list matrix.
##
## The specifics are somewhat tricky.  We thus use tailor-made explicit
## transformations for certains kinds of nodes, with the underlying idea
## that transforming a list of "same" nodes will result in a list matrix
## with rows corresponding to transforming a single node.  We thus
## arrange for the transformation methods to always work in a list
## context (i.e., to be vectorized) and return list matrices, and the
## transformation function to maintain the context.

oaih_transform <-
function(x)
{
    list_type_node_names <-
        c("ListIdentifiers", "ListMetadataFormats", "ListRecords",
          "ListSets")
    xml_node_classes <- c("xml_node")
    if(inherits(x, xml_node_classes)) {
        if(!is.na(match(xml_name(x), list_type_node_names)))
            .oaih_vtransform(xml_children(x))
        else
            .oaih_vtransform(list(x))[1L, ]
    }
    else {
        if(!is.list(x))
            stop("Can only transform lists and XML nodes")
        else if(all(vapply(x, is.character, TRUE)))
            return(x)
        else if(!all(vapply(x, inherits, TRUE, xml_node_classes)))
            stop("Can only transform lists of XML nodes or character vectors")
        .oaih_vtransform(x)
    }
}

## Only allow for lists of same-kind nodes for now.

.oaih_vtransform <-
function(x)
{
    if(length(nm <- unique(sapply(x, xml_name))) > 1L)
        stop("Cannot transform mixed-kind node lists")
    if(is.null(trafo <- oaih_transform_methods_db[[nm]]))
        stop(gettextf("Cannot transform node kind '%s'", nm),
             domain = NA)
    trafo(x)
}

oaih_transform_methods_db <- new.env()

oaih_transform_methods_db$Identify <-
function(x)
{
    ## A list of <Identify> nodes.
    ## See <https://www.openarchives.org/OAI/2.0/OAI-PMH.xsd> for the
    ## definition of identifyType: this
    ## * MUST include one instance of:
    ##     repositoryName baseURL protocolVersion earliestDatestamp
    ##     deletedRecord granularity
    ## * MUST include one or more instances of
    ##     adminEmail
    ## * MAY include multiple instances of the following OPTIONAL
    ##   elements:
    ##     compression description
    ## i.e.,
    ##    repositoryName baseURL protocolVersion earliestDatestamp
    ##    adminEmail+ compression* description*
    ## where description can be anything and everything else is string
    ## type.
    Identify_vars <-
        c("repositoryName", "baseURL", "protocolVersion",
          "earliestDatestamp", "deletedRecord", "granularity",
          "adminEmail", "compression")
    cbind(.get_value_of_any_text_nodes(x, Identify_vars),
          description =
          lapply(x, .get_all_any_nodes, "description"))
}

## .oaih_vtransform_branding <-
## function(x)
## {
##     ## A list of <branding> nodes using a schema for collection branding
##     ## within OAI.
##     ## See <https://www.openarchives.org/OAI/2.0/branding.xsd> for the
##     ## definition of branding nodes: a sequence with elements
##     ##   collectionIcon? metadataRendering*
##     ## which in turn are sequences with elements
##     ##   url link? title? width? height?
##     ## (the first three string-type, the last two integer) and of base
##     ## URLs with attributes metadataNamespace and mimeType.
##     ## But what can this usefully be transformed into?
##     ## So let's leave it for now ...
## }

oaih_transform_methods_db$dc <-
function(x)
{
    ## Dublin Core:
    ## A list of <oai:dc> nodes.
    ## See <https://www.openarchives.org/OAI/2.0/oai_dc.xsd> for the
    ## definition of oai_dcType.
    ## Note that each of the 15 string-type variables can occur
    ## arbitrarly often in an oai_dc metadata node.
    dc_vars <-
        c("title", "creator", "subject", "description", "publisher",
          "contributor", "date", "type", "format", "identifier",
          "source", "language", "relation", "coverage", "rights")
    .get_value_of_any_text_nodes(x, dc_vars)
}

oaih_transform_methods_db$eprints <-
function(x)
{
    ## A list of <eprint> nodes, for use in the description section of
    ## an Identify() reply, defined by the e-print community.
    ## See <https://www.openarchives.org/OAI/2.0/eprints.xsd> for the
    ## definition of eprints:eprintsDescriptionType: a sequence with
    ## elements
    ##   content? metadataPolicy dataPolicy submissionPolicy comment*
    ## with the last a string and the others of TextURLType, consisting
    ## of an URL and text.
    ## Grr, so we must flatten this out ... ugly.

    eprint_textURL_vars <-
        c("content", "metadataPolicy", "dataPolicy", "submissionPolicy")

    out <- lapply(x,
                  function(e) {
                      kids <- xml_children(e)
                      vals <- rep.int(list(rep.int(NA_character_, 2L)),
                                      4L)
                      pos <- match(eprint_textURL_vars,
                                   xml_name(kids),
                                   nomatch = 0L)
                      vals[pos > 0L] <-
                          lapply(kids[pos],
                                 function(e) {
                                     k <- xml_children(e)
                                     xml_text(k[match(c("URL", "text"),
                                                      xml_name(k),
                                                      nomatch = 0)])[1L : 2L]
                                 })
                      vals
                  })
    ## out <- lapply(x,
    ##               function(n)
    ##               lapply(n[eprint_textURL_vars],
    ##                      function(x)
    ##                      c(.xml_value_if_not_null(x[["URL"]],
    ##                                               NA_character_),
    ##                        .xml_value_if_not_null(x[["text"]],
    ##                                               NA_character_)
    ##                        )))
    out <- matrix(as.list(do.call(rbind, lapply(out, unlist))),
                  nrow = length(x), ncol = 8L,
                  dimnames =
                  list(NULL,
                       c(t(outer(eprint_textURL_vars, c("URL", "text"),
                                 paste, sep = ".")))))
    cbind(out, .get_value_of_any_text_nodes(x, "comment"))
}

oaih_transform_methods_db$friends <-
function(x)
{
    ## A list of <friend> nodes, for use in the description section of
    ## an Identify() reply for repositories that list "friends"
    ## (confederate data providers).
    ## See <https://www.openarchives.org/OAI/2.0/friends.xsd> for the
    ## definition of friends:friendsType: a sequence with elements
    ##   baseURL*
    ## (string-type).
    .get_value_of_any_text_nodes(x, "baseURL")
}

oaih_transform_methods_db$header <-
function(x)
{
    ## A list of <header> nodes.
    ## See <https://www.openarchives.org/OAI/2.0/OAI-PMH.xsd> for the
    ## definition of headerType: a sequence with elements
    ##   identifier datestamp setSpec*
    ## and optionally a status attribute (all string-type).
    ## <FIXME>
    ## Add support for the status attribute.
    ## </FIXME>
    cbind(.get_value_of_unique_text_nodes(x, c("identifier",
                                               "datestamp")),
          setSpec =
          lapply(x,
                 function(kid)
                     vapply(.xml_children_named(kid, "setSpec"),
                            xml_text,
                            ""))
          )
}

oaih_transform_methods_db$metadataFormat <-
function(x)
{
    ## A list of <metadataFormat> nodes.
    ## See <https://www.openarchives.org/OAI/2.0/OAI-PMH.xsd> for the
    ## definition of metadataFormatType: a sequence with elements
    ##   metadataPrefix schema metadataNamespace
    ## (all string-type).
    metadataFormat_vars <-
        c("metadataPrefix", "schema", "metadataNamespace")
    .get_value_of_unique_text_nodes(x, metadataFormat_vars)
}

oaih_transform_methods_db$`oai-identifier` <-
function(x)
{
    ## A list of <oai-identifier> nodes, for use in the description
    ## section of an Identify() reply.
    ## See <https://www.openarchives.org/OAI/2.0/oai-identifier.xsd> for
    ## the definition of oai-identifierType: a sequence with elements
    ##   scheme repositoryIdentifier delimiter sampleIdentifier
    ## (all string-type).
    oai_identifier_vars <-
        c("scheme", "repositoryIdentifier", "delimiter",
          "sampleIdentifier")
    .get_value_of_unique_text_nodes(x, oai_identifier_vars)
}

oaih_transform_methods_db$rfc1807 <-
function(x)
{
    ## A list of <rfc1807> metadata nodes.
    ## See <https://www.openarchives.org/OAI/rfc1807.xsd> for the
    ## definition of fc1807:rfc1807Type: a sequence with the elements in
    ## rfc1807_vars below, where the first three occur exactly once and
    ## the others arbitrarily often (all text-type).
    rfc1807_vars <-
        c("bib-version", "id", "entry", "organization", "title", "type",
          "revision", "withdraw", "author", "corp-author", "contact",
          "date", "pages", "copyright", "handle", "other_access",
          "retrieval", "keyword", "cr-category", "period", "series",
          "monitoring", "funding", "contract", "grant", "language",
          "notes", "abstract")
    .get_value_of_any_text_nodes(x, rfc1807_vars)
}

oaih_transform_methods_db$record <-
function(x)
{
    ## A list of <record> nodes.
    ## See <https://www.openarchives.org/OAI/2.0/OAI-PMH.xsd> for the
    ## definition of recordType: a sequence with elements
    ##   header metadata? about*
    ## where metadata and about can be "anything".
    cbind(.oaih_vtransform(lapply(x, .xml_child_named, "header")),
          metadata = lapply(x, .get_one_any_node, "metadata"),
          about = lapply(x, .get_all_any_nodes, "about"))
}

oaih_transform_methods_db$set <-
function(x)
{
    ## A list of <set> nodes.
    ## See <https://www.openarchives.org/OAI/2.0/OAI-PMH.xsd> for the
    ## definition of setType: a sequence with elements
    ##   setSpec setName setDescription*
    ## where setDescription can be "anything".
    cbind(.get_value_of_unique_text_nodes(x, c("setSpec", "setName")),
          setDescription =
          lapply(x, .get_all_any_nodes, "setDescription"))
}
