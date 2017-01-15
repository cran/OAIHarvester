oaih_apply <-
function(x, FUN, ..., drop = TRUE)
{
    y <- lapply(x, FUN, ...)
    if(drop && all(lengths(y) == 1L))
        y <- unlist(y, recursive = FALSE)
    if(is.array(x))
        y <- array(y, dim = dim(x), dimnames = dimnames(x))
    y
}
