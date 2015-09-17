require(XML)
require(RCurl)
require(magrittr)

url <- "http://advanceddataanalytics.net/ebooks/"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)

links1 <- links[grepl(pattern = 'pdf', x = tolower(links))] %>% as.character

for(i in 1:10){
  url <- links1[i]
  urlsplit <- strsplit(links1[i], '/')[[1]]
  name <- urlsplit[urlsplit %>% length]
  download.file(url, name)
