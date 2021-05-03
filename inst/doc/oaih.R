### R code from vignette source 'oaih.Rnw'

###################################################
### code chunk number 1: oaih.Rnw:196-199
###################################################
library("OAIHarvester")
baseurl <- "http://epub.wu.ac.at/cgi/oai2"
if(inherits(tryCatch(oaih_identify(baseurl), error = identity), "error")) q()


###################################################
### code chunk number 2: oaih.Rnw:203-206
###################################################
require("xml2")
options(warnPartialMatchArgs = FALSE)
options(width = 80)


###################################################
### code chunk number 3: oaih.Rnw:211-213
###################################################
x <- oaih_identify(baseurl)
rbind(x, deparse.level = 0)


###################################################
### code chunk number 4: oaih.Rnw:219-220
###################################################
sapply(x$description, xml_name)


###################################################
### code chunk number 5: oaih.Rnw:224-225
###################################################
oaih_transform(x$description[[1L]])


###################################################
### code chunk number 6: oaih.Rnw:230-233
###################################################
oaih_list_metadata_formats(baseurl)
sets <- oaih_list_sets(baseurl)
rbind(head(sets, 3L), tail(sets, 3L))


###################################################
### code chunk number 7: oaih.Rnw:239-240
###################################################
spec <- unlist(sets[sets[, "setName"] == "Type = Thesis", "setSpec"])


###################################################
### code chunk number 8: oaih.Rnw:243-244
###################################################
x <- oaih_list_records(baseurl, set = spec)


###################################################
### code chunk number 9: oaih.Rnw:248-250
###################################################
dim(x)
colnames(x)


###################################################
### code chunk number 10: oaih.Rnw:255-258
###################################################
m <- x[, "metadata"]
m <- oaih_transform(m[lengths(m) > 0L])
dim(m)


###################################################
### code chunk number 11: oaih.Rnw:261-262
###################################################
colnames(m)


###################################################
### code chunk number 12: oaih.Rnw:314-315
###################################################
m[c(1L, 6L, 7L), "subject"]


###################################################
### code chunk number 13: oaih.Rnw:319-324
###################################################
sep <- "[[:space:]]*/[[:space:]]*"
keywords_by_thesis <-
    strsplit(unlist(lapply(m[, "subject"],  paste, collapse = " / ")),
             sep)
keywords <- unlist(keywords_by_thesis)


###################################################
### code chunk number 14: oaih.Rnw:328-330
###################################################
counts <- table(keywords)
table(counts)


###################################################
### code chunk number 15: oaih.Rnw:334-335
###################################################
sort(counts[counts >= 3L], decreasing = TRUE)


###################################################
### code chunk number 16: oaih.Rnw:339-340
###################################################
counts["R"]


###################################################
### code chunk number 17: oaih.Rnw:343-346
###################################################
lapply(m[sapply(keywords_by_thesis, function(kw) any(kw == "R")),
         c("title", "creator")],
       strwrap)


###################################################
### code chunk number 18: oaih.Rnw:351-353
###################################################
m[grep("^Feinerer", unlist(m[, "creator"])),
  c("title", "creator", "subject")]


