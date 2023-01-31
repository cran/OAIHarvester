### R code from vignette source 'oaih.Rnw'

###################################################
### code chunk number 1: oaih.Rnw:197-199
###################################################
library("OAIHarvester")
baseurl <- "https://research.wu.ac.at/ws/oai"


###################################################
### code chunk number 2: oaih.Rnw:203-207
###################################################
if(inherits(tryCatch(oaih_identify(baseurl), error = identity), "error")) q()
require("xml2")
options(warnPartialMatchArgs = FALSE)
options(width = 80)


###################################################
### code chunk number 3: oaih.Rnw:212-214
###################################################
x <- oaih_identify(baseurl)
rbind(x, deparse.level = 0)


###################################################
### code chunk number 4: oaih.Rnw:220-221
###################################################
vapply(x$description, xml_name, "")


###################################################
### code chunk number 5: oaih.Rnw:225-226
###################################################
oaih_transform(x$description[[2L]])


###################################################
### code chunk number 6: oaih.Rnw:231-234
###################################################
oaih_list_metadata_formats(baseurl)
sets <- oaih_list_sets(baseurl)
rbind(head(sets, 3L), tail(sets, 3L))


###################################################
### code chunk number 7: oaih.Rnw:239-240
###################################################
x <- oaih_list_records(baseurl, set = "publications:year2005")


###################################################
### code chunk number 8: oaih.Rnw:244-246
###################################################
dim(x)
colnames(x)


###################################################
### code chunk number 9: oaih.Rnw:251-254
###################################################
m <- x[, "metadata"]
m <- oaih_transform(m[lengths(m) > 0L])
dim(m)


###################################################
### code chunk number 10: oaih.Rnw:257-258
###################################################
colnames(m)


###################################################
### code chunk number 11: oaih.Rnw:269-270
###################################################
m[head(which(lengths(m[, "subject"]) > 0), 3L), "subject"]


###################################################
### code chunk number 12: oaih.Rnw:275-277
###################################################
keywords <- unlist(m[, "subject"])
keywords <- keywords[!startsWith(keywords, "/dk/atira/pure")]


###################################################
### code chunk number 13: oaih.Rnw:281-283
###################################################
counts <- table(keywords)
table(counts)


###################################################
### code chunk number 14: oaih.Rnw:287-288
###################################################
sort(counts[counts >= 10L], decreasing = TRUE)


###################################################
### code chunk number 15: oaih.Rnw:294-297
###################################################
pos <- which(vapply(m[, "creator"],
                    function(e) any(startsWith(e, "Hornik")),
                    NA))


###################################################
### code chunk number 16: oaih.Rnw:301-302
###################################################
unlist(m[pos, "title"])


###################################################
### code chunk number 17: oaih.Rnw:305-306
###################################################
table(unlist(m[pos, "type"]))


###################################################
### code chunk number 18: oaih.Rnw:309-310
###################################################
pos <- pos[lengths(m[pos, "subject"]) > 0L]


###################################################
### code chunk number 19: oaih.Rnw:313-314
###################################################
unique(m[pos, "subject"])


