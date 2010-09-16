###################################################
### chunk number 1: 
###################################################
#line 194 "oaih.Rnw"
library("OAIHarvester")
baseurl <- "http://epub.wu.ac.at/cgi/oai2"


###################################################
### chunk number 2: 
###################################################
#line 200 "oaih.Rnw"
require("XML")
options(warnPartialMatchArgs = FALSE)
options(width = 80)


###################################################
### chunk number 3: 
###################################################
#line 208 "oaih.Rnw"
x <- oaih_identify(baseurl)
rbind(x, deparse.level = 0)


###################################################
### chunk number 4: 
###################################################
#line 216 "oaih.Rnw"
sapply(x$description, xmlName)


###################################################
### chunk number 5: 
###################################################
#line 221 "oaih.Rnw"
oaih_transform(x$description[[1L]])


###################################################
### chunk number 6: 
###################################################
#line 227 "oaih.Rnw"
oaih_list_metadata_formats(baseurl)
sets <- oaih_list_sets(baseurl)
rbind(head(sets, 3L), tail(sets, 3L))


###################################################
### chunk number 7: 
###################################################
#line 236 "oaih.Rnw"
spec <- unlist(sets[sets[, "setName"] == "Type = Thesis", "setSpec"])


###################################################
### chunk number 8: 
###################################################
#line 240 "oaih.Rnw"
x <- oaih_list_records(baseurl, set = spec)


###################################################
### chunk number 9: 
###################################################
#line 245 "oaih.Rnw"
dim(x)
colnames(x)


###################################################
### chunk number 10: 
###################################################
#line 252 "oaih.Rnw"
m <- x[, "metadata"]
m <- oaih_transform(m[sapply(m, length) > 0L])
dim(m)


###################################################
### chunk number 11: 
###################################################
#line 258 "oaih.Rnw"
colnames(m)


###################################################
### chunk number 12: 
###################################################
#line 311 "oaih.Rnw"
m[c(2L, 5L, 6L), "subject"]


###################################################
### chunk number 13: 
###################################################
#line 316 "oaih.Rnw"
sep <- "[[:space:]]*/[[:space:]]*"
keywords_by_thesis <- strsplit(unlist(m[, "subject"]), sep)
keywords <- unlist(keywords_by_thesis)


###################################################
### chunk number 14: 
###################################################
#line 323 "oaih.Rnw"
counts <- table(keywords)
table(counts)


###################################################
### chunk number 15: 
###################################################
#line 329 "oaih.Rnw"
sort(counts[counts >= 3L], decreasing = TRUE)


###################################################
### chunk number 16: 
###################################################
#line 334 "oaih.Rnw"
counts["R"]


###################################################
### chunk number 17: 
###################################################
#line 338 "oaih.Rnw"
m[sapply(keywords_by_thesis, function(kw) any(kw == "R")),
  c("title", "creator")]


###################################################
### chunk number 18: 
###################################################
#line 345 "oaih.Rnw"
m[grep("^Feinerer", unlist(m[, "creator"])),
  c("title", "creator", "subject")]


