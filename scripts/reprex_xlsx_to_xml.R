

# https://stackoverflow.com/questions/35234863/how-convert-a-data-frame-into-a-xml-file-with-r
data<- structure(list(page = c("Page One", "Page Two"), base64 = c("Very Long String thats a base64 character", "Very Long String thats a base64 character")), .Names = c("page", "base64"), row.names = 1:2, class = "data.frame")
names(data) <- c("title", "page")

library(XML)
xml <- xmlTree()
# names(xml) # list out the functions available in xmlTree()
xml$addTag("report", close=FALSE, attrs=c(type="enhanced"))
xml$addTag("pages", close=FALSE)
for (i in 1:nrow(data)) {
  xml$addTag("page", close=FALSE)
  for (j in names(data)) {
    xml$addTag(j, data[i, j])
  }
  xml$closeTag()
}
xml$closeTag()
xml$closeTag()
cat(saveXML(xml))
