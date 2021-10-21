oaih_providers <-
local({
    .providers <- NULL
    function() {
        if(is.null(.providers)) {
            providers <-
                html_tables("https://www.openarchives.org/Register/BrowseSites")
            providers <- providers[[1L]][, 3L : 5L]
            names(providers) <- c("name", "baseurl", "identifier")
            .providers <<- providers
        }
        .providers
    }
})
