## Helper functions.

.get_value_of_unique_text_nodes <-
function(x, names = NULL)
{
    names(x) <- NULL
    if(is.null(names))
        names <- unique(unlist(lapply(x, xmlApply, xmlName)))
    out <- lapply(x,
                  function(n) {
                      lapply(n[names],
                             .xml_value_if_not_null,
                             NA_character_)
                  })
    matrix(unlist(out, recursive = FALSE),
           nrow = length(x), ncol = length(names),
           byrow = TRUE, dimnames = list(NULL, names))    
}

.get_value_of_any_text_nodes <-
function(x, names = NULL)
{
    if(is.null(names))
        names <- unique(unlist(lapply(x, xmlApply, xmlName)))
    ## <NOTE>
    ## We used to eventually apply
    ##    as.character(sapply(.elements_named(e, nm), xmlValue)))
    ## but apparently xmlValue() gives character() rather than an empty
    ## string for "empty" nodes ... eventually resulting in
    ##    "character(0)"
    ## entries.  Argh.  Let's use .xml_value_if_not_empty() instead.
    out <-
        do.call(cbind,
                lapply(names,
                       function(nm) {
                           lapply(x, 
                                  function(e)
                                  as.character(sapply(.elements_named(e,
                                                                      nm),
                                                      .xml_value_if_not_empty))
                                  )
                       }))
    dimnames(out) <- list(NULL, names)
    out
}

.elements_named <-
function(x, name)
    structure(x[names(x) == name], names = NULL)

## <FIXME>
## Is the distinction between getting a single "any" node and all "any"
## nodes still useful?
## </FIXME>

.get_one_any_node <-
function(x, name)
    x[[name]][[1L]]

.get_all_any_nodes <-
function(x, name)
    lapply(.elements_named(x, name), `[[`, 1L)

.xml_value_if_not_null <-
function(n, default)
    if(!is.null(n)) xmlValue(n) else default

.xml_value_if_not_empty <-
function(n)
    if(length(v <- xmlValue(n))) v else ""
