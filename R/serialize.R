oaih_xml_to_str <-
function(x)
    UseMethod("oaih_xml_to_str")

oaih_xml_to_str.xml_node <-
function(x)
    structure(as.character(as_xml_document(x)),
              class = "oaih_str_from_xml_node")

oaih_xml_to_str.xml_nodeset <-
function(x)    
    structure(as.character(as_xml_document(x, "root")),
              class = "oaih_str_from_xml_nodeset")

oaih_xml_to_str.default <-
function(x)
{
    if(is.list(x))
        x[] <- lapply(x, oaih_xml_to_str)
    x
}

oaih_str_to_xml <-
function(x)
    UseMethod("oaih_str_to_xml")

oaih_str_to_xml.oaih_str_from_xml_node <-
function(x)
    xml_find_first(read_xml(unclass(x)), "/node()")

oaih_str_to_xml.oaih_str_from_xml_nodeset <-
function(x)
    xml_find_all(read_xml(unclass(x)), "/*/node()")

oaih_str_to_xml.default <-
function(x)    
{
    if(is.list(x))
        x[] <- lapply(x, oaih_str_to_xml)
    x
}

oaih_save_RDS <-
function(x, ...)
    saveRDS(oaih_xml_to_str(x), ...)

oaih_read_RDS <-
function(file, ...)
    oaih_str_to_xml(readRDS(file, ...))
